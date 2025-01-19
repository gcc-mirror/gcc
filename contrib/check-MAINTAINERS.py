#!/usr/bin/env python3

# Copyright (C) 2022-2025 Free Software Foundation, Inc.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING.  If not, write to
# the Free Software Foundation, 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

# Check that names in the file are sorted
# alphabetically by surname.

import locale
import sys
from difflib import ndiff
from itertools import groupby

import unidecode

locale.setlocale(locale.LC_ALL, 'en_US.utf8')

exit_code = 0

if len(sys.argv) != 2:
    print('Usage: ./check-MAINTAINERS.py path-to/MAINTAINERS')
    sys.exit(1)


def get_surname(name):
    parts = name.split()
    surname = parts[-1]

    # Special-case some names
    if name == 'Stefan Schulze Frielinghaus':
        surname = parts[1]
    elif name == 'Kris Van Hees':
        surname = parts[1]
    elif surname == "d'Humieres":
        surname = 'Humieres'

    # Remove accents
    return unidecode.unidecode(surname)


def check_group(name, lines, columns):
    global exit_code

    named_lines = []
    for line in lines:
        if line.startswith(' '):
            print(f'Line should not start with space: "{line}"')
            exit_code = 2
            continue

        if line.endswith(' '):
            print(f'Line should not end with space: "{line}"')
            exit_code = 3
            continue

        # Special-case some names
        if line == 'James Norris':
            named_lines.append((get_surname(line), line + "\n"))
            continue

        pieces = []
        for i, column in enumerate(columns):
            piece = ""
            if len(line) <= column:
                print(f'Line too short: "{line}"')
                exit_code = 4
            elif column > 0 and line[column - 1] != ' ':
                print(f'Column {column - 1} should be empty: "{line}"')
                exit_code = 5
            elif line[column] == ' ':
                print(f'Column {column} should be nonempty: "{line}"')
                exit_code = 6
            elif i == len(columns) - 1:
                piece = line[column:].rstrip()
            else:
                piece = line[column:columns[i + 1]].rstrip()

            if "  " in piece:
                print(f'Malformed field at column {column}: "{line}"')
                exit_code = 7

            pieces.append(piece)

        named_lines.append((get_surname(pieces[0]), line + "\n"))

        email = pieces[-1]
        if email and (not email.startswith('<') or not email.endswith('>')):
            print(f'Malformed email address: "{line}"')
            exit_code = 8

    lines = [line + "\n" for line in lines]
    sorted_lines = [line for _, line in sorted(named_lines)]
    if lines != sorted_lines:
        exit_code = 1
        diff = ndiff(lines, sorted_lines)
        print(f'Wrong order for {name}:\n')
        print(''.join(diff))
    else:
        print(f'{name} are fine!')


text = open(sys.argv[1]).read()
if '\t' in text:
    print('The file should not contain tabs')
    exit_code = 9

sections = [
    # heading, paragraph index, column numbers
    ('Global Reviewers', 1, [0, 48]),
    ('Write After Approval', 2, [0, 32, 48]),
    ('Bug database only accounts', 1, [0, 48]),
    ('Contributing under the DCO', 2, [0, 48])
]

i = 0
count = 0
for is_empty, lines in groupby(text.splitlines(), lambda x: not x):
    if is_empty:
        continue
    lines = list(lines)
    if count > 0:
        count -= 1
        if count == 0:
            check_group(sections[i][0], lines, sections[i][2])
            i += 1
    elif len(lines) == 1 and i < len(sections) and sections[i][0] in lines[0]:
        count = sections[i][1]

if i < len(sections):
    print(f'Missing "{sections[i][0]}" section')
    exit_code = 10

sys.exit(exit_code)
