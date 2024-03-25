#!/usr/bin/env python3

# Copyright (C) 2022-2024 Free Software Foundation, Inc.
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
from itertools import dropwhile, takewhile

import unidecode

locale.setlocale(locale.LC_ALL, 'en_US.utf8')

exit_code = 0

if len(sys.argv) != 2:
    print('Usage: ./check-MAINTAINERS.py path-to/MAINTAINERS')
    sys.exit(1)


def sort_by_surname(line):
    name = line.split('\t')[0]
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
    return (unidecode.unidecode(surname), line)


def has_tab(line):
    return '\t' in line


def is_empty(line):
    return line


def check_group(name, lines):
    global exit_code

    for line in lines:
        if line.startswith(' '):
            print(f'Line should not start with space: "{line}"')
            exit_code = 2

    lines = [line + '\n' for line in lines]
    sorted_lines = sorted(lines, key=sort_by_surname)
    if lines != sorted_lines:
        exit_code = 1
        diff = ndiff(lines, sorted_lines)
        print(f'Wrong order for {name}:\n')
        print(''.join(diff))
    else:
        print(f'{name} are fine!')


lines = open(sys.argv[1]).read().splitlines()

needle = 'Global Reviewers'
lines = list(dropwhile(lambda x: x.strip() != needle, lines))
lines = lines[2:]

chunk = list(takewhile(is_empty, lines))
check_group(needle, chunk)

needle = 'Write After Approval'
lines = list(dropwhile(lambda x: needle not in x, lines))
lines = lines[2:]

chunk = list(takewhile(is_empty, lines))
check_group(needle, chunk)

needle = 'Bug database only accounts'
lines = list(dropwhile(lambda x: needle not in x, lines))
lines = lines[2:]

chunk = list(takewhile(is_empty, lines))
check_group(needle, chunk)

needle = 'Contributing under the DCO'
lines = list(dropwhile(lambda x: needle not in x, lines))[1:]
lines = list(dropwhile(lambda x: not has_tab(x), lines))
check_group(needle, lines)

sys.exit(exit_code)
