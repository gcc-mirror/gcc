#!/usr/bin/env python3
#
# Script to analyze warnings produced by rtags command (using LLVM):
# rc --diagnose-all --synchronous-diagnostics --json
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# GCC is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.  */
#
#
#

import sys
import json
import argparse

def skip_warning(filename, warning):
    ignores = {
            '': ['-Warray-bounds', '-Wmismatched-tags', 'gcc_gfc: -Wignored-attributes', '-Wchar-subscripts',
                'string literal (potentially insecure): -Wformat-security', '-Wdeprecated-register',
                '-Wvarargs', 'keyword is hidden by macro definition', "but the argument has type 'char *': -Wformat-pedantic",
                '-Wnested-anon-types', 'qualifier in explicit instantiation of', 'attribute argument not supported: asm_fprintf'],
            'insn-modes.c': ['-Wshift-count-overflow'],
            'insn-emit.c': ['-Wtautological-compare'],
            'insn-attrtab.c': ['-Wparentheses-equality'],
            'gimple-match.c': ['-Wunused-', '-Wtautological-compare'],
            'generic-match.c': ['-Wunused-', '-Wtautological-compare'],
    }

    message = warning['message']

    if warning['type'] == 'fixit':
        return True

    for name, ignores in ignores.items():
        for i in ignores:
            if name in filename and i in message:
                return True

    return False

parser = argparse.ArgumentParser()
parser.add_argument('json_file', help = 'Rtags JSON file with diagnostics')
parser.add_argument('-n', '--no-filter', action = 'store_true', help = 'No filter')

args = parser.parse_args()

data = json.load(open(args.json_file))
file_warnings = data['checkStyle']

total = 0
for filename, warnings in file_warnings.items():
    if warnings:
        for w in warnings:
            if args.no_filter or not skip_warning(filename, w):
                total += 1
                print('%s:%d:%d:%s' % (filename, w['line'], w['column'], w['message']))

print('Total: %d' % total)
