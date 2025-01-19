# Copyright (C) 2020-2025 Free Software Foundation, Inc.

# This file is part of GCC.

# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.

# GCC is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# Run this program as
# 	python ./make-rust-unicode.py UnicodeData.txt \
#       DerivedNormalizationProps.txt DerivedCoreProperties.txt \
#       > rust-unicode-data.h

import sys
from typing import Tuple

Codepoint = int
Range = Tuple[Codepoint, Codepoint]

COPYRIGHT = (
    "// Copyright (C) 2020-2025 Free Software Foundation, Inc.\n"
    "\n"
    "// This file is part of GCC.\n"
    "\n"
    "// GCC is free software; you can redistribute it and/or modify it under\n"
    "// the terms of the GNU General Public License as published by the Free\n"
    "// Software Foundation; either version 3, or (at your option) any later\n"
    "// version.\n"
    "\n"
    "// GCC is distributed in the hope that it will be useful, but WITHOUT ANY\n"
    "// WARRANTY; without even the implied warranty of MERCHANTABILITY or\n"
    "// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License\n"
    "// for more details.\n"
    "\n"
    "// You should have received a copy of the GNU General Public License\n"
    "// along with GCC; see the file COPYING3.  If not see\n"
    "// <http://www.gnu.org/licenses/>."
)

# Decomposition_Mapping table
decomposition_map: dict[Codepoint, list[Codepoint]] = {}
# Canonical_Combining_Class table
ccc_table: dict[Codepoint, int] = {}
# Ranges of codepoints with the Full_Composition_Exclusion property
composition_exclusion_ranges: list[Range] = []
# Ranges of codepoints with the Full_Composition_Exclusion property
alphabetic_ranges: list[Range] = []
# Ranges of codepoints with NFC_QC=No
nfc_qc_no_ranges: list[Range] = []
# Ranges of codepoints with NFC_QC=Maybe
nfc_qc_maybe_ranges: list[Range] = []
numeric_codepoints: list[Codepoint] = []

# Note that an element of range `[m, n]` (a list in python) represents [m, n)


def binary_search_ranges(ranges: list[Range], target: Codepoint) -> int:
    low: int = 0
    high: int = len(ranges) - 1
    while low <= high:
        mid = (low + high) // 2
        start, end = ranges[mid]
        if start <= target <= end - 1:
            return mid  # target found. returns index.
        elif target < start:
            high = mid - 1
        else:
            low = mid + 1
    # target not found.
    return -1


# Utility function to parse '<codepoint>...<codepoint>' or '<codepoint>'
def parse_codepoint_range(range_str: str) -> Range:
    codepoint_range: list[str] = range_str.split("..")
    assert len(codepoint_range) == 1 or len(codepoint_range) == 2, "Invalid format"
    start_cp, end_cp = 0, 0
    if len(codepoint_range) == 1:
        # m..n => [m, n+1)
        start_cp = int(codepoint_range[0], 16)
        end_cp = start_cp + 1
    else:
        # m => [m, m+1)
        start_cp = int(codepoint_range[0], 16)
        end_cp = int(codepoint_range[1], 16) + 1
    return start_cp, end_cp


def read_unicode_data_txt(filepath: str) -> None:
    def process_line(line: str) -> None:
        rows = line.split(";")
        if len(rows) != 15:
            return
        # Parse codepoint
        cp = int(rows[0], 16)
        # Parse general category
        category = rows[2]
        if category == "Nd" or category == "Nl" or category == "No":
            numeric_codepoints.append(cp)

        # Parse CCC
        ccc = int(rows[3], 10)
        if ccc != 0:
            ccc_table[cp] = ccc
        # Parse decomposition mapping
        # Ignore compatibility decomposition mapping because
        # it is not required for **NFC** normalization.
        if not rows[5].startswith("<"):
            decomp_cp_strs = rows[5].split(" ")
            decomp_cps = []
            for s in decomp_cp_strs:
                if s == "":
                    continue
                decomp_cps.append(int(s, 16))
            assert (
                len(decomp_cps) <= 2
            ), "Decomposition_Mapping must not contain more than 2 characters."
            if len(decomp_cps) > 0:
                decomposition_map[cp] = decomp_cps

    with open(filepath, "r", encoding="UTF-8") as file:
        while line := file.readline():
            process_line(line.rstrip())


def read_derived_norm_props_txt(filepath: str) -> None:
    def process_line(line) -> None:
        # Ignore comments
        line = line.split("#")[0]
        rows = line.split(";")
        # Too few rows. Skipped.
        if len(rows) < 2:
            return
        rows[0] = rows[0].lstrip().rstrip()
        rows[1] = rows[1].lstrip().rstrip()
        cp_range = parse_codepoint_range(rows[0])
        if rows[1] == "Full_Composition_Exclusion":
            composition_exclusion_ranges.append(cp_range)
        elif rows[1] == "NFC_QC":
            assert len(rows) >= 3, "Too few rows for NFC_QC"
            rows[2] = rows[2].lstrip().rstrip()
            if rows[2] == "N":
                nfc_qc_no_ranges.append(cp_range)
            elif rows[2] == "M":
                nfc_qc_maybe_ranges.append(cp_range)
            else:
                raise RuntimeError("Value of NFC_QC must be N or M")

    with open(filepath, "r", encoding="UTF-8") as file:
        while line := file.readline():
            process_line(line.rstrip())


def read_derived_core_props_txt(filepath: str) -> None:
    def process_line(line: str) -> None:
        # Ignore comments
        line = line.split("#")[0]
        rows = line.split(";")
        # Too few rows. Skipped.
        if len(rows) < 2:
            return
        rows[0] = rows[0].lstrip().rstrip()
        rows[1] = rows[1].lstrip().rstrip()
        if rows[1] != "Alphabetic":
            return
        cp_range: Range = parse_codepoint_range(rows[0])
        alphabetic_ranges.append(cp_range)

    with open(filepath, "r", encoding="UTF-8") as file:
        while line := file.readline():
            process_line(line.rstrip())


def write_decomposition() -> None:
    print("const std::map<uint32_t, std::vector<uint32_t>> DECOMPOSITION_MAP = {")
    print("  // clang-format off")
    for cp in sorted(decomposition_map):
        print("  {{{:#06x}, ".format(cp), end="")
        print("{", end="")
        for decomp_cp in decomposition_map[cp]:
            print("{:#06x}, ".format(decomp_cp), end="")
        print("}},")
    print("  // clang-format on")
    print("};")


def write_recomposition() -> None:
    print(
        "const std::map<std::pair<uint32_t, uint32_t>, uint32_t> RECOMPOSITION_MAP = {{"
    )
    print("  // clang-format off")
    for cp in decomposition_map:
        if binary_search_ranges(composition_exclusion_ranges, cp) != -1:
            continue
        d1: Codepoint
        d2: Codepoint
        if len(decomposition_map[cp]) == 1:
            d1 = decomposition_map[cp][0]
            d2 = 0
        else:
            d1 = decomposition_map[cp][0]
            d2 = decomposition_map[cp][1]
        print("  {{{{{:#06x}, {:#06x}}}, {:#06x}}},".format(d1, d2, cp))
    print("  // clang-format on")
    print("}};")


def write_ccc() -> None:
    print("const std::map<uint32_t, int32_t> CCC_TABLE = {")
    print("  // clang-format off")
    for cp in ccc_table:
        print("  {{{:#06x}, {}}},".format(cp, ccc_table[cp]))
    print("  // clang-format on")
    print("};")


def write_alphabetic() -> None:
    print(
        "const std::array<std::pair<uint32_t, uint32_t>, NUM_ALPHABETIC_RANGES> ALPHABETIC_RANGES = {{"
    )
    print("  // clang-format off")
    for r in alphabetic_ranges:
        print("  {{{:#06x}, {:#06x}}},".format(r[0], r[1]))
    print("  // clang-format on")
    print("}};")


def write_numeric() -> None:
    print("const std::array<uint32_t, NUM_NUMERIC_CODEPOINTS> NUMERIC_CODEPOINTS = {{")
    print("  // clang-format off")
    for i, cp in enumerate(numeric_codepoints):
        if i % 16 == 0:
            print("  ", end="")
        print("{:#06x}, ".format(cp), end="")
        if i % 16 == 15:
            print()
    if i % 16 != 15:
        print()
    print("  // clang-format on")
    print("}};")


def write_nfc_qc():
    print(
        "const std::array<std::pair<uint32_t, uint32_t>, {}> NFC_QC_NO_RANGES = {{{{".format(
            len(nfc_qc_no_ranges)
        )
    )
    print("  // clang-format off")
    for r in nfc_qc_no_ranges:
        print("  {{{:#06x}, {:#06x}}},".format(r[0], r[1]))
    print("  // clang-format on")
    print("}};")

    print(
        "const std::array<std::pair<uint32_t, uint32_t>, {}> NFC_QC_MAYBE_RANGES = {{{{".format(
            len(nfc_qc_maybe_ranges)
        )
    )
    print("  // clang-format off")
    for r in nfc_qc_maybe_ranges:
        print("  {{{:#06x}, {:#06x}}},".format(r[0], r[1]))
    print("  // clang-format on")
    print("}};")


def main() -> None:
    if len(sys.argv) != 4:
        print("too few arguments", file=sys.stderr)
        exit(-1)
    unicode_txt_path: str = sys.argv[1]
    norm_props_txt_path: str = sys.argv[2]
    core_props_txt_path: str = sys.argv[3]

    read_unicode_data_txt(unicode_txt_path)
    read_derived_norm_props_txt(norm_props_txt_path)
    read_derived_core_props_txt(core_props_txt_path)

    print(COPYRIGHT)
    print()

    print('#include "rust-system.h"\n')
    print("namespace Rust {\n")
    print("const uint32_t NUM_ALPHABETIC_RANGES = {};".format(len(alphabetic_ranges)))
    print(
        "const uint32_t NUM_NUMERIC_CODEPOINTS = {};\n".format(len(numeric_codepoints))
    )

    write_decomposition()
    print()
    write_recomposition()
    print()
    write_ccc()
    print()
    write_alphabetic()
    print()
    write_numeric()
    print()
    write_nfc_qc()
    print()

    print("} // namespace Rust")


if __name__ == "__main__":
    main()
