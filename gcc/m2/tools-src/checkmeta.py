#!/usr/bin/env python3

# utility to check meta errors for simple format spec mistakes.

# Copyright (C) 2016-2024 Free Software Foundation, Inc.
#
# This file is part of GNU Modula-2.
#
# GNU Modula-2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GNU Modula-2 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Modula-2; see the file COPYING.  If not, write to the
# Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

import argparse
import os
import pathlib
import sys


exit_code = 0


def visit_dir(directory, ext, func):
    # visit_dir - call func for each file below, dir, matching extension, ext.
    list_of_files = os.listdir(directory)
    list_of_files.sort()
    for filename in list_of_files:
        path = pathlib.Path(filename)
        full = os.path.join(directory, filename)
        if path.suffix == ext:
            func(full)


def check_format_spec(filename, line, no):
    global exit_code

    percent = line.find('%')
    if percent >= 0:
        specifier = False
        for ch in line[percent:]:
            if ch in ['{', '%']:
                pass
            elif ch in ['1', '2', '3', '4']:
                if specifier:
                    sys.stderr.write('%s:%d: format specifier error, the symbol position digit must be before the specifier: %s\n' % (filename, no, line))
                    exit_code = 1
            else:
                specifier = True


def search_format(filename, line, no):
    cbra = line.find('{')
    while cbra >= 0:
        colon = line.find(':', cbra)
        end = line.find('}', cbra)
        if end >= 0:
            if (colon >= 0) and (colon < end):
                end = colon
            check_format_spec(filename, line[cbra:end], no)
            cbra = line.find('{', end)
        else:
            return


def check_string_quote (filename, line, no, quote):
    end = line.find(quote, 1)
    if end > 0:
        search_format(filename, line[1:end], no)


def check_string (filename, line, no):
    quote = line.find("'")
    if quote >= 0:
        check_string_quote(filename, line[quote:], no, "'")
    quote = line.find('"')
    if quote >= 0:
        check_string_quote(filename, line[quote:], no, '"')


def check_meta_spec (filename):
    lines = open(filename).readlines()
    extra = 0
    for no, line in enumerate(lines):
        if extra > 0:
            extra -= 1
            check_string(filename, line, no+1)
        elif "Meta" in line:
            meta = line.find("Meta")
            if meta >= 0:
                bra = line.find("(", meta)
                if bra >= 0:
                    check_string(filename, line[bra:], no+1)
            extra = 1


def handle_arguments():
    # handle_arguments create and return the args object.
    parser = argparse.ArgumentParser()
    parser.add_argument('-s', '--srcdir',
                        help='set source directory.',
                        default='.', action='store')
    args = parser.parse_args()
    return args


def main():
    args = handle_arguments()
    visit_dir(args.srcdir, '.mod', check_meta_spec)
    visit_dir(args.srcdir, '.bnf', check_meta_spec)
    sys.exit(exit_code)


main()
