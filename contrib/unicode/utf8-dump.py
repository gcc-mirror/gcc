#!/usr/bin/env python3
#
# Script to dump a UTF-8 file as a list of numbered lines (mimicking GCC's
# diagnostic output format), interleaved with lines per character showing
# the Unicode codepoints, the UTF-8 encoding bytes, the name of the
# character, and, where printable, the characters themselves.
# The lines are printed in logical order, which may help the reader to grok
# the relationship between visual and logical ordering in bi-di files.
#
# SPDX-License-Identifier: MIT
#
# Copyright (C) 2021 David Malcolm <dmalcolm@redhat.com>.
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT
# OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import sys
import unicodedata


def get_name(ch):
    try:
        return unicodedata.name(ch)
    except ValueError:
        if ch == '\n':
            return 'LINE FEED (LF)'
        return '(unknown)'


def get_printable(ch):
    cat = unicodedata.category(ch)
    if cat == 'Cc':
        return '(control character)'
    elif cat == 'Cf':
        return '(format control)'
    elif cat[0] == 'Z':
        return '(separator)'
    return ch


def dump_file(f_in):
    line_num = 1
    for line in f_in:
        print('%4i | %s' % (line_num, line.rstrip()))
        for ch in line:
            utf8_desc = '%15s' % (' '.join(['0x%02x' % b
                                            for b in ch.encode('utf-8')]))
            print('%4s |   U+%04X %s %40s %s'
                  % ('', ord(ch), utf8_desc, get_name(ch), get_printable(ch)))
        line_num += 1


with open(sys.argv[1], mode='r') as f_in:
    dump_file(f_in)
