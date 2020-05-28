#!/usr/bin/env python3

# Copyright (C) 2020 Free Software Foundation, Inc.
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

# This script parses a .diff file generated with 'diff -up' or 'diff -cp'
# and adds a skeleton ChangeLog file to the file. It does not try to be
# too smart when parsing function names, but it produces a reasonable
# approximation.
#
# Author: Martin Liska <mliska@suse.cz>

import argparse
import os
import re
import sys
from itertools import takewhile

import requests

from unidiff import PatchSet

pr_regex = re.compile(r'(\/(\/|\*)|[Cc*!])\s+(?P<pr>PR [a-z+-]+\/[0-9]+)')
dr_regex = re.compile(r'(\/(\/|\*)|[Cc*!])\s+(?P<dr>DR [0-9]+)')
identifier_regex = re.compile(r'^([a-zA-Z0-9_#].*)')
comment_regex = re.compile(r'^\/\*')
struct_regex = re.compile(r'^(class|struct|union|enum)\s+'
                          r'(GTY\(.*\)\s+)?([a-zA-Z0-9_]+)')
macro_regex = re.compile(r'#\s*(define|undef)\s+([a-zA-Z0-9_]+)')
super_macro_regex = re.compile(r'^DEF[A-Z0-9_]+\s*\(([a-zA-Z0-9_]+)')
fn_regex = re.compile(r'([a-zA-Z_][^()\s]*)\s*\([^*]')
template_and_param_regex = re.compile(r'<[^<>]*>')
bugzilla_url = 'https://gcc.gnu.org/bugzilla/rest.cgi/bug?id=%s&' \
               'include_fields=summary'

function_extensions = set(['.c', '.cpp', '.C', '.cc', '.h', '.inc', '.def'])

help_message = """\
Generate ChangeLog template for PATCH.
PATCH must be generated using diff(1)'s -up or -cp options
(or their equivalent in git).
"""

script_folder = os.path.realpath(__file__)
gcc_root = os.path.dirname(os.path.dirname(script_folder))


def find_changelog(path):
    folder = os.path.split(path)[0]
    while True:
        if os.path.exists(os.path.join(gcc_root, folder, 'ChangeLog')):
            return folder
        folder = os.path.dirname(folder)
        if folder == '':
            return folder
    raise AssertionError()


def extract_function_name(line):
    if comment_regex.match(line):
        return None
    m = struct_regex.search(line)
    if m:
        # Struct declaration
        return m.group(1) + ' ' + m.group(3)
    m = macro_regex.search(line)
    if m:
        # Macro definition
        return m.group(2)
    m = super_macro_regex.search(line)
    if m:
        # Supermacro
        return m.group(1)
    m = fn_regex.search(line)
    if m:
        # Discard template and function parameters.
        fn = m.group(1)
        fn = re.sub(template_and_param_regex, '', fn)
        return fn.rstrip()
    return None


def try_add_function(functions, line):
    fn = extract_function_name(line)
    if fn and fn not in functions:
        functions.append(fn)
    return bool(fn)


def sort_changelog_files(changed_file):
    return (changed_file.is_added_file, changed_file.is_removed_file)


def get_pr_titles(prs):
    output = ''
    for pr in prs:
        id = pr.split('/')[-1]
        r = requests.get(bugzilla_url % id)
        bugs = r.json()['bugs']
        if len(bugs) == 1:
            output += '%s - %s\n' % (pr, bugs[0]['summary'])
            print(output)
    if output:
        output += '\n'
    return output


def generate_changelog(data, no_functions=False, fill_pr_titles=False):
    changelogs = {}
    changelog_list = []
    prs = []
    out = ''
    diff = PatchSet(data)

    for file in diff:
        changelog = find_changelog(file.path)
        if changelog not in changelogs:
            changelogs[changelog] = []
            changelog_list.append(changelog)
        changelogs[changelog].append(file)

        # Extract PR entries from newly added tests
        if 'testsuite' in file.path and file.is_added_file:
            for line in list(file)[0]:
                m = pr_regex.search(line.value)
                if m:
                    pr = m.group('pr')
                    if pr not in prs:
                        prs.append(pr)
                else:
                    m = dr_regex.search(line.value)
                    if m:
                        dr = m.group('dr')
                        if dr not in prs:
                            prs.append(dr)
                    else:
                        break

    if fill_pr_titles:
        out += get_pr_titles(prs)

    # sort ChangeLog so that 'testsuite' is at the end
    for changelog in sorted(changelog_list, key=lambda x: 'testsuite' in x):
        files = changelogs[changelog]
        out += '%s:\n' % os.path.join(changelog, 'ChangeLog')
        out += '\n'
        for pr in prs:
            out += '\t%s\n' % pr
        # new and deleted files should be at the end
        for file in sorted(files, key=sort_changelog_files):
            assert file.path.startswith(changelog)
            in_tests = 'testsuite' in changelog or 'testsuite' in file.path
            relative_path = file.path[len(changelog):].lstrip('/')
            functions = []
            if file.is_added_file:
                msg = 'New test' if in_tests else 'New file'
                out += '\t* %s: %s.\n' % (relative_path, msg)
            elif file.is_removed_file:
                out += '\t* %s: Removed.\n' % (relative_path)
            elif hasattr(file, 'is_rename') and file.is_rename:
                out += '\t* %s: Moved to...\n' % (relative_path)
                new_path = file.target_file[2:]
                # A file can be theoretically moved to a location that
                # belongs to a different ChangeLog.  Let user fix it.
                if new_path.startswith(changelog):
                    new_path = new_path[len(changelog):].lstrip('/')
                out += '\t* %s: ...here.\n' % (new_path)
            else:
                if not no_functions:
                    for hunk in file:
                        # Do not add function names for testsuite files
                        extension = os.path.splitext(relative_path)[1]
                        if not in_tests and extension in function_extensions:
                            last_fn = None
                            modified_visited = False
                            success = False
                            for line in hunk:
                                m = identifier_regex.match(line.value)
                                if line.is_added or line.is_removed:
                                    if not line.value.strip():
                                        continue
                                    modified_visited = True
                                    if m and try_add_function(functions,
                                                              m.group(1)):
                                        last_fn = None
                                        success = True
                                elif line.is_context:
                                    if last_fn and modified_visited:
                                        try_add_function(functions, last_fn)
                                        last_fn = None
                                        modified_visited = False
                                        success = True
                                    elif m:
                                        last_fn = m.group(1)
                                        modified_visited = False
                            if not success:
                                try_add_function(functions,
                                                 hunk.section_header)
                if functions:
                    out += '\t* %s (%s):\n' % (relative_path, functions[0])
                    for fn in functions[1:]:
                        out += '\t(%s):\n' % fn
                else:
                    out += '\t* %s:\n' % relative_path
        out += '\n'
    return out


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=help_message)
    parser.add_argument('input', nargs='?',
                        help='Patch file (or missing, read standard input)')
    parser.add_argument('-s', '--no-functions', action='store_true',
                        help='Do not generate function names in ChangeLogs')
    parser.add_argument('-p', '--fill-up-bug-titles', action='store_true',
                        help='Download title of mentioned PRs')
    parser.add_argument('-c', '--changelog',
                        help='Append the ChangeLog to a git commit message '
                             'file')
    args = parser.parse_args()
    if args.input == '-':
        args.input = None

    input = open(args.input) if args.input else sys.stdin
    data = input.read()
    output = generate_changelog(data, args.no_functions,
                                args.fill_up_bug_titles)
    if args.changelog:
        lines = open(args.changelog).read().split('\n')
        start = list(takewhile(lambda l: not l.startswith('#'), lines))
        end = lines[len(start):]
        with open(args.changelog, 'w') as f:
            if start:
                # appent empty line
                if start[-1] != '':
                    start.append('')
            else:
                # append 2 empty lines
                start = 2 * ['']
            f.write('\n'.join(start))
            f.write('\n')
            f.write(output)
            f.write('\n'.join(end))
    else:
        print(output, end='')
