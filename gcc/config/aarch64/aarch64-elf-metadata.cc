/* ELF metadata for AArch64 architecture.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_STRING
#define INCLUDE_ALGORITHM
#define INCLUDE_MEMORY
#define INCLUDE_VECTOR
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "output.h"

#include "aarch64-elf-metadata.h"

/* Defined for convenience.  */
#define POINTER_BYTES (POINTER_SIZE / BITS_PER_UNIT)

namespace aarch64 {

constexpr unsigned GNU_PROPERTY_AARCH64_FEATURE_1_AND = 0xc0000000;
constexpr unsigned GNU_PROPERTY_AARCH64_FEATURE_1_BTI = (1U << 0);
constexpr unsigned GNU_PROPERTY_AARCH64_FEATURE_1_PAC = (1U << 1);
constexpr unsigned GNU_PROPERTY_AARCH64_FEATURE_1_GCS = (1U << 2);

namespace {

std::string
gnu_property_features_to_string (unsigned feature_1_and)
{
  struct flag_name
  {
    unsigned int mask;
    const char *name;
  };

  static const flag_name flags[] = {
    {GNU_PROPERTY_AARCH64_FEATURE_1_BTI, "BTI"},
    {GNU_PROPERTY_AARCH64_FEATURE_1_PAC, "PAC"},
    {GNU_PROPERTY_AARCH64_FEATURE_1_GCS, "GCS"},
  };

  const char *separator = "";
  std::string s_features;
  for (auto &flag : flags)
    if (feature_1_and & flag.mask)
      {
	s_features.append (separator).append (flag.name);
	separator = ", ";
      }
  return s_features;
};

} // namespace anonymous

section_note_gnu_property::section_note_gnu_property ()
  : m_feature_1_and (0) {}

void
section_note_gnu_property::bti_enabled ()
{
  m_feature_1_and |= GNU_PROPERTY_AARCH64_FEATURE_1_BTI;
}

void
section_note_gnu_property::pac_enabled ()
{
  m_feature_1_and |= GNU_PROPERTY_AARCH64_FEATURE_1_PAC;
}

void
section_note_gnu_property::gcs_enabled ()
{
  m_feature_1_and |= GNU_PROPERTY_AARCH64_FEATURE_1_GCS;
}

void
section_note_gnu_property::write () const
{
  if (m_feature_1_and)
    {
      /* Generate .note.gnu.property section.  */
      switch_to_section (
	get_section (".note.gnu.property", SECTION_NOTYPE, NULL));

      /* PT_NOTE header: namesz, descsz, type.
	 namesz = 4 ("GNU\0")
	 descsz = 16 (Size of the program property array)
		  [(12 + padding) * Number of array elements]
	 type   = 5 (NT_GNU_PROPERTY_TYPE_0).  */
      assemble_align (POINTER_SIZE);
      assemble_integer (GEN_INT (4), 4, 32, 1);
      assemble_integer (GEN_INT (ROUND_UP (12, POINTER_BYTES)), 4, 32, 1);
      assemble_integer (GEN_INT (5), 4, 32, 1);

      /* PT_NOTE name.  */
      assemble_string ("GNU", 4);

      /* PT_NOTE contents for NT_GNU_PROPERTY_TYPE_0:
	 type   = GNU_PROPERTY_AARCH64_FEATURE_1_AND
	 datasz = 4
	 data   = feature_1_and.  */
      fputs (integer_asm_op (4, true), asm_out_file);
      fprint_whex (asm_out_file, GNU_PROPERTY_AARCH64_FEATURE_1_AND);
      putc ('\n', asm_out_file);
      assemble_integer (GEN_INT (4), 4, 32, 1);

      fputs (integer_asm_op (4, true), asm_out_file);
      fprint_whex (asm_out_file, m_feature_1_and);
      if (flag_debug_asm)
	{
	  auto const &s_features
	    = gnu_property_features_to_string (m_feature_1_and);
	  asm_fprintf (asm_out_file,
		       "\t%s GNU_PROPERTY_AARCH64_FEATURE_1_AND (%s)\n",
		       ASM_COMMENT_START, s_features.c_str ());
	}
      else
	putc ('\n', asm_out_file);

      /* Pad the size of the note to the required alignment.  */
      assemble_align (POINTER_SIZE);
    }
}

} // namespace aarch64
