#!/usr/bin/env python3
#
# boilerplate.py utility to rewrite the boilerplate with new dates.
#
# Copyright (C) 2018-2023 Free Software Foundation, Inc.
# Contributed by Gaius Mulley <gaius@glam.ac.uk>.
#
# This file is part of GNU Modula-2.
#
# GNU Modula-2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GNU Modula-2 is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Modula-2; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.
#

import argparse
import datetime
import os
import sys


error_count = 0
seen_files = []
output_name = None

ISO_COPYRIGHT = 'Copyright ISO/IEC'
COPYRIGHT = 'Copyright (C)'
GNU_PUBLIC_LICENSE = 'GNU General Public License'
GNU_LESSER_GENERAL = 'GNU Lesser General'
GCC_RUNTIME_LIB_EXC = 'GCC Runtime Library Exception'
VERSION_2_1 = 'version 2.1'
VERSION_2 = 'version 2'
VERSION_3 = 'version 3'
Licenses = {VERSION_2_1: 'v2.1', VERSION_2: 'v2', VERSION_3: 'v3'}
CONTRIBUTED_BY = 'ontributed by'


def printf(fmt, *args):
    # printf - keeps C programmers happy :-)
    print(str(fmt) % args, end=' ')


def error(fmt, *args):
    # error - issue an error message.
    global error_count

    print(str(fmt) % args, end=' ')
    error_count += 1


def halt_on_error():
    if error_count > 0:
        os.sys.exit(1)


def basename(f):
    b = f.split('/')
    return b[-1]


def analyse_comment(text, f):
    # analyse_comment determine the license from the top comment.
    start_date, end_date = None, None
    contribution, summary, lic = None, None, None
    if text.find(ISO_COPYRIGHT) > 0:
        lic = 'BSISO'
        now = datetime.datetime.now()
        for d in range(1984, now.year+1):
            if text.find(str(d)) > 0:
                if start_date is None:
                    start_date = str(d)
                end_date = str(d)
        return start_date, end_date, '', '', lic
    elif text.find(COPYRIGHT) > 0:
        if text.find(GNU_PUBLIC_LICENSE) > 0:
            lic = 'GPL'
        elif text.find(GNU_LESSER_GENERAL) > 0:
            lic = 'LGPL'
        for license_ in Licenses.keys():
            if text.find(license_) > 0:
                lic += Licenses[license_]
        if text.find(GCC_RUNTIME_LIB_EXC) > 0:
            lic += 'x'
        now = datetime.datetime.now()
        for d in range(1984, now.year+1):
            if text.find(str(d)) > 0:
                if start_date is None:
                    start_date = str(d)
                end_date = str(d)
        if text.find(CONTRIBUTED_BY) > 0:
            i = text.find(CONTRIBUTED_BY)
            i += len(CONTRIBUTED_BY)
            j = text.index('. ', i)
            contribution = text[i:j]
    if text.find(basename(f)) > 0:
        i = text.find(basename(f))
        j = text.find('. ', i)
        if j < 0:
            error("summary of the file does not finish with a '.'")
            summary = text[i:]
        else:
            summary = text[i:j]
    return start_date, end_date, contribution, summary, lic


def analyse_header_without_terminator(f, start):
    text = ''
    for count, l in enumerate(open(f).readlines()):
        parts = l.split(start)
        if len(parts) > 1:
            line = start.join(parts[1:])
            line = line.strip()
            text += ' '
            text += line
        elif (l.rstrip() != '') and (len(parts[0]) > 0):
            return analyse_comment(text, f), count
    return [None, None, None, None, None], 0


def analyse_header_with_terminator(f, start, end):
    inComment = False
    text = ''
    for count, line in enumerate(open(f).readlines()):
        while line != '':
            line = line.strip()
            if inComment:
                text += ' '
                pos = line.find(end)
                if pos >= 0:
                    text += line[:pos]
                    line = line[pos:]
                    inComment = False
                else:
                    text += line
                    line = ''
            else:
                pos = line.find(start)
                if (pos >= 0) and (len(line) > len(start)):
                    before = line[:pos].strip()
                    if before != '':
                        return analyse_comment(text, f), count
                    line = line[pos + len(start):]
                    inComment = True
                elif (line != '') and (line == end):
                    line = ''
                else:
                    return analyse_comment(text, f), count
    return [None, None, None, None, None], 0


def analyse_header(f, start, end):
    # analyse_header -
    if end is None:
        return analyse_header_without_terminator(f, start)
    else:
        return analyse_header_with_terminator(f, start, end)


def add_stop(sentence):
    # add_stop - add a full stop to a sentance.
    if sentence is None:
        return None
    sentence = sentence.rstrip()
    if (len(sentence) > 0) and (sentence[-1] != '.'):
        return sentence + '.'
    return sentence


GPLv3 = """
%s

Copyright (C) %s Free Software Foundation, Inc.
Contributed by %s

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.
"""

GPLv3x = """
%s

Copyright (C) %s Free Software Foundation, Inc.
Contributed by %s

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.
"""

LGPLv3 = """
%s

Copyright (C) %s Free Software Foundation, Inc.
Contributed by %s

This file is part of GNU Modula-2.

GNU Modula-2 is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with GNU Modula-2.  If not, see <https://www.gnu.org/licenses/>.
"""

BSISO = """
Library module defined by the International Standard
   Information technology - programming languages
   BS ISO/IEC 10514-1:1996E Part 1: Modula-2, Base Language.

   Copyright ISO/IEC (International Organization for Standardization
   and International Electrotechnical Commission) %s.

   It may be freely copied for the purpose of implementation (see page
   707 of the Information technology - Programming languages Part 1:
   Modula-2, Base Language.  BS ISO/IEC 10514-1:1996).
"""

templates = {}
templates['GPLv3'] = GPLv3
templates['GPLv3x'] = GPLv3x
templates['LGPLv3'] = LGPLv3
templates['LGPLv2.1'] = LGPLv3
templates['BSISO'] = BSISO


def write_template(fo, magic, start, end, dates, contribution, summary, lic):
    if lic in templates:
        if lic == 'BSISO':
            # non gpl but freely distributed for the implementation of a
            # compiler
            text = templates[lic] % (dates)
            text = text.rstrip()
        else:
            summary = summary.lstrip()
            contribution = contribution.lstrip()
            summary = add_stop(summary)
            contribution = add_stop(contribution)
            if magic is not None:
                fo.write(magic)
                fo.write('\n')
            text = templates[lic] % (summary, dates, contribution)
            text = text.rstrip()
        if end is None:
            text = text.split('\n')
            for line in text:
                fo.write(start)
                fo.write(' ')
                fo.write(line)
                fo.write('\n')
        else:
            text = text.lstrip()
            fo.write(start)
            fo.write(' ')
            fo.write(text)
            fo.write('  ')
            fo.write(end)
            fo.write('\n')
        # add a blank comment line for a script for eye candy.
        if start == '#' and end is None:
            fo.write(start)
            fo.write('\n')
    else:
        error('no template found for: %s\n', lic)
        os.sys.exit(1)
    return fo


def write_boiler_plate(fo, magic, start, end,
                       start_date, end_date, contribution, summary, gpl):
    if start_date == end_date:
        dates = start_date
    else:
        dates = '%s-%s' % (start_date, end_date)
    return write_template(fo, magic, start, end,
                          dates, contribution, summary, gpl)


def rewrite_file(f, magic, start, end, start_date, end_date,
                 contribution, summary, gpl, lines):
    text = ''.join(open(f).readlines()[lines:])
    if output_name == '-':
        fo = sys.stdout
    else:
        fo = open(f, 'w')
    fo = write_boiler_plate(fo, magic, start, end,
                            start_date, end_date, contribution, summary, gpl)
    fo.write(text)
    fo.flush()
    if output_name != '-':
        fo.close()


def handle_header(f, magic, start, end):
    # handle_header keep reading lines of file, f, looking for start, end
    # sequences and comments inside.  The comments are checked for:
    # date, contribution, summary
    global error_count

    error_count = 0
    [start_date, end_date,
     contribution, summary, lic], lines = analyse_header(f, start, end)
    if lic is None:
        error('%s:1:no GPL found at the top of the file\n', f)
    else:
        if args.verbose:
            printf('copyright: %s\n', lic)
            if (start_date is not None) and (end_date is not None):
                if start_date == end_date:
                    printf('dates = %s\n', start_date)
                else:
                    printf('dates = %s-%s\n', start_date, end_date)
            if summary is not None:
                printf('summary: %s\n', summary)
            if contribution is not None:
                printf('contribution: %s\n', contribution)
        if start_date is None:
            error('%s:1:no date found in the GPL at the top of the file\n', f)
        if args.contribution is None:
            if contribution == '':
                error('%s:1:no contribution found in the ' +
                      'GPL at the top of the file\n', f)
            else:
                contribution = args.contribution
        if summary is None:
            if args.summary == '':
                error('%s:1:no single line summary found in the ' +
                      'GPL at the top of the file\n', f)
            else:
                summary = args.summary
    if error_count == 0:
        now = datetime.datetime.now()
        if args.no:
            print(f, 'suppressing change as requested: %s-%s %s'
                  % (start_date, end_date, lic))
        else:
            if lic == 'BSISO':
                # don't change the BS ISO license!
                pass
            elif args.extensions:
                lic = 'GPLv3x'
            elif args.gpl3:
                lic = 'GPLv3'
            rewrite_file(f, magic, start, end, start_date,
                         str(now.year), contribution, summary, lic, lines)
    else:
        printf('too many errors, no modifications will occur\n')


def bash_tidy(f):
    # bash_tidy tidy up dates using '#' comment
    handle_header(f, '#!/bin/bash', '#', None)


def python_tidy(f):
    # python_tidy tidy up dates using '#' comment
    handle_header(f, '#!/usr/bin/env python3', '#', None)


def bnf_tidy(f):
    # bnf_tidy tidy up dates using '--' comment
    handle_header(f, None, '--', None)


def c_tidy(f):
    # c_tidy tidy up dates using '/* */' comments
    handle_header(f, None, '/*', '*/')


def m2_tidy(f):
    # m2_tidy tidy up dates using '(* *)' comments
    handle_header(f, None, '(*', '*)')


def in_tidy(f):
    # in_tidy tidy up dates using '#' as a comment and check
    # the first line for magic number.
    first = open(f).readlines()[0]
    if (len(first) > 0) and (first[:2] == '#!'):
        # magic number found, use this
        handle_header(f, first, '#', None)
    else:
        handle_header(f, None, '#', None)


def do_visit(args, dirname, names):
    # do_visit helper function to call func on every extension file.
    global output_name
    func, extension = args
    for f in names:
        if len(f) > len(extension) and f[-len(extension):] == extension:
            output_name = f
            func(os.path.join(dirname, f))


def visit_dir(startDir, ext, func):
    # visit_dir call func for each file in startDir which has ext.
    global output_name, seen_files
    for dirName, subdirList, fileList in os.walk(startDir):
        for fname in fileList:
            if (len(fname) > len(ext)) and (fname[-len(ext):] == ext):
                fullpath = os.path.join(dirName, fname)
                output_name = fullpath
                if not (fullpath in seen_files):
                    seen_files += [fullpath]
                    func(fullpath)
            # Remove the first entry in the list of sub-directories
            # if there are any sub-directories present
        if len(subdirList) > 0:
            del subdirList[0]


def find_files():
    # find_files for each file extension call the appropriate tidy routine.
    visit_dir(args.recursive, '.h.in', c_tidy)
    visit_dir(args.recursive, '.in', in_tidy)
    visit_dir(args.recursive, '.sh', in_tidy)
    visit_dir(args.recursive, '.py', python_tidy)
    visit_dir(args.recursive, '.c', c_tidy)
    visit_dir(args.recursive, '.h', c_tidy)
    visit_dir(args.recursive, '.cc', c_tidy)
    visit_dir(args.recursive, '.def', m2_tidy)
    visit_dir(args.recursive, '.mod', m2_tidy)
    visit_dir(args.recursive, '.bnf', bnf_tidy)


def handle_arguments():
    # handle_arguments create and return the args object.
    parser = argparse.ArgumentParser()
    parser.add_argument('-c', '--contribution',
                        help='set the contribution string ' +
                        'at the top of the file.',
                        default='', action='store')
    parser.add_argument('-d', '--debug', help='turn on internal debugging.',
                        default=False, action='store_true')
    parser.add_argument('-f', '--force',
                        help='force a check to insist that the ' +
                        'contribution, summary and GPL exist.',
                        default=False, action='store_true')
    parser.add_argument('-g', '--gplv3', help='change to GPLv3',
                        default=False, action='store_true')
    parser.add_argument('-o', '--outputfile', help='set the output file',
                        default='-', action='store')
    parser.add_argument('-r', '--recursive',
                        help='recusively scan directory for known file ' +
                        'extensions (.def, .mod, .c, .h, .py, .in, .sh).',
                        default='.', action='store')
    parser.add_argument('-s', '--summary',
                        help='set the summary line for the file.',
                        default=None, action='store')
    parser.add_argument('-u', '--update', help='update all dates.',
                        default=False, action='store_true')
    parser.add_argument('-v', '--verbose',
                        help='display copyright, ' +
                        'date and contribution messages',
                        action='store_true')
    parser.add_argument('-x', '--extensions',
                        help='change to GPLv3 with GCC runtime extensions.',
                        default=False, action='store_true')
    parser.add_argument('-N', '--no',
                        help='do not modify any file.',
                        action='store_true')
    args = parser.parse_args()
    return args


def has_ext(name, ext):
    # has_ext return True if, name, ends with, ext.
    if len(name) > len(ext):
        return name[-len(ext):] == ext
    return False


def single_file(name):
    # single_file scan the single file for a GPL boilerplate which
    # has a GPL, contribution field and a summary heading.
    if has_ext(name, '.def') or has_ext(name, '.mod'):
        m2_tidy(name)
    elif has_ext(name, '.h') or has_ext(name, '.c') or has_ext(name, '.cc'):
        c_tidy(name)
    elif has_ext(name, '.in'):
        in_tidy(name)
    elif has_ext(name, '.sh'):
        in_tidy(name)  # uses magic number for actual sh/bash
    elif has_ext(name, '.py'):
        python_tidy(name)


def main():
    # main - handle_arguments and then find source files.
    global args, output_name
    args = handle_arguments()
    output_name = args.outputfile
    if args.recursive:
        find_files()
    elif args.inputfile is None:
        print('an input file must be specified on the command line')
    else:
        single_file(args.inputfile)
    halt_on_error()


main()
