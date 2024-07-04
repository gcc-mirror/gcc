#!/usr/bin/env python3

# Copyright (C) 2020-2024 Free Software Foundation, Inc.
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
import datetime
import json
import os
import re
import subprocess
import sys
from itertools import takewhile

import requests

from unidiff import PatchSet

LINE_LIMIT = 100
TAB_WIDTH = 8

# Initial commit:
#   +--------------------------------------------------+
#   | gccrs: Some title                                |
#   |                                                  | This is the "start"
#   | This is some text explaining the commit.         |
#   | There can be several lines.                      |
#   |                                                  |<------------------->
#   | Signed-off-by: My Name <my@mail.com>             | This is the "end"
#   +--------------------------------------------------+
#
# Results in:
#   +--------------------------------------------------+
#   | gccrs: Some title                                |
#   |                                                  |
#   | This is some text explaining the commit.         | This is the "start"
#   | There can be several lines.                      |
#   |                                                  |<------------------->
#   | gcc/rust/ChangeLog:                              |
#   |                                                  | This is the generated
#   |         * some_file (bla):                       | ChangeLog part
#   |         (foo):                                   |
#   |                                                  |<------------------->
#   | Signed-off-by: My Name <my@mail.com>             | This is the "end"
#   +--------------------------------------------------+

# this regex matches the first line of the "end" in the initial commit message
FIRST_LINE_OF_END_RE = re.compile('(?i)^(signed-off-by:|co-authored-by:|#)')

pr_regex = re.compile(r'(\/(\/|\*)|[Cc*!])\s+(?P<pr>PR [a-z+-]+\/[0-9]+)')
prnum_regex = re.compile(r'PR (?P<comp>[a-z+-]+)/(?P<num>[0-9]+)')
dr_regex = re.compile(r'(\/(\/|\*)|[Cc*!])\s+(?P<dr>DR [0-9]+)')
dg_regex = re.compile(r'{\s+dg-(error|warning)')
pr_filename_regex = re.compile(r'(^|[\W_])[Pp][Rr](?P<pr>\d{4,})')
identifier_regex = re.compile(r'^([a-zA-Z0-9_#].*)')
comment_regex = re.compile(r'^\/\*')
struct_regex = re.compile(r'^(class|struct|union|enum)\s+'
                          r'(GTY\(.*\)\s+)?([a-zA-Z0-9_]+)')
macro_regex = re.compile(r'#\s*(define|undef)\s+([a-zA-Z0-9_]+)')
super_macro_regex = re.compile(r'^DEF[A-Z0-9_]+\s*\(([a-zA-Z0-9_]+)')
fn_regex = re.compile(r'([a-zA-Z_][^()\s]*)\s*\([^*]')
template_and_param_regex = re.compile(r'<[^<>]*>')
md_def_regex = re.compile(r'\(define.*\s+"(.*)"')
bugzilla_url = 'https://gcc.gnu.org/bugzilla/rest.cgi/bug?id=%s&' \
               'include_fields=summary,component'

function_extensions = {'.c', '.cpp', '.C', '.cc', '.h', '.inc', '.def', '.md'}

# NB: Makefile.in isn't listed as it's not always generated.
generated_files = {'aclocal.m4', 'config.h.in', 'configure'}

help_message = """\
Generate ChangeLog template for PATCH.
PATCH must be generated using diff(1)'s -up or -cp options
(or their equivalent in git).
"""

script_folder = os.path.realpath(__file__)
root = os.path.dirname(os.path.dirname(script_folder))


def find_changelog(path):
    folder = os.path.split(path)[0]
    while True:
        if os.path.exists(os.path.join(root, folder, 'ChangeLog')):
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
    output = []
    for idx, pr in enumerate(prs):
        pr_id = pr.split('/')[-1]
        r = requests.get(bugzilla_url % pr_id)
        bugs = r.json()['bugs']
        if len(bugs) == 1:
            prs[idx] = 'PR %s/%s' % (bugs[0]['component'], pr_id)
            out = '%s - %s\n' % (prs[idx], bugs[0]['summary'])
            if out not in output:
                output.append(out)
    if output:
        output.append('')
    return '\n'.join(output)


def append_changelog_line(out, relative_path, text):
    line = f'\t* {relative_path}:'
    if len(line.replace('\t', ' ' * TAB_WIDTH) + ' ' + text) <= LINE_LIMIT:
        out += f'{line} {text}\n'
    else:
        out += f'{line}\n'
        out += f'\t{text}\n'
    return out


def get_rel_path_if_prefixed(path, folder):
    if path.startswith(folder):
        return path[len(folder):].lstrip('/')
    else:
        return path


def generate_changelog(data, no_functions=False, fill_pr_titles=False,
                       additional_prs=None):
    global prs
    prs = []

    changelogs = {}
    changelog_list = []
    out = ''
    diff = PatchSet(data)

    if additional_prs:
        for apr in additional_prs:
            if not apr.startswith('PR ') and '/' in apr:
                apr = 'PR ' + apr
            if apr not in prs:
                prs.append(apr)
    for file in diff:
        # skip files that can't be parsed
        if file.path == '/dev/null':
            continue
        changelog = find_changelog(file.path)
        if changelog not in changelogs:
            changelogs[changelog] = []
            changelog_list.append(changelog)
        changelogs[changelog].append(file)

        # Extract PR entries from newly added tests
        if 'testsuite' in file.path and file.is_added_file:
            # Only search first ten lines as later lines may
            # contains commented code which a note that it
            # has not been tested due to a certain PR or DR.
            this_file_prs = []
            hunks = list(file)
            if hunks:
                for line in hunks[0][0:10]:
                    m = pr_regex.search(line.value)
                    if m:
                        pr = m.group('pr')
                        if pr not in prs:
                            prs.append(pr)
                            this_file_prs.append(pr.split('/')[-1])
                    else:
                        m = dr_regex.search(line.value)
                        if m:
                            dr = m.group('dr')
                            if dr not in prs:
                                prs.append(dr)
                                this_file_prs.append(dr.split('/')[-1])
                        elif dg_regex.search(line.value):
                            # Found dg-warning/dg-error line
                            break

            # PR number in the file name
            fname = os.path.basename(file.path)
            m = pr_filename_regex.search(fname)
            if m:
                pr = m.group('pr')
                pr2 = 'PR ' + pr
                if pr not in this_file_prs and pr2 not in prs:
                    prs.append(pr2)

    if fill_pr_titles:
        out += get_pr_titles(prs)

    # print list of PR entries before ChangeLog entries
    if prs:
        if not out:
            out += '\n'
        for pr in prs:
            out += '\t%s\n' % pr
        out += '\n'

    # sort ChangeLog so that 'testsuite' is at the end
    for changelog in sorted(changelog_list, key=lambda x: 'testsuite' in x):
        files = changelogs[changelog]
        out += '%s:\n' % os.path.join(changelog, 'ChangeLog')
        out += '\n'
        # new and deleted files should be at the end
        for file in sorted(files, key=sort_changelog_files):
            assert file.path.startswith(changelog)
            in_tests = 'testsuite' in changelog or 'testsuite' in file.path
            relative_path = get_rel_path_if_prefixed(file.path, changelog)
            functions = []
            if file.is_added_file:
                msg = 'New test.' if in_tests else 'New file.'
                out = append_changelog_line(out, relative_path, msg)
            elif file.is_removed_file:
                out = append_changelog_line(out, relative_path, 'Removed.')
            elif hasattr(file, 'is_rename') and file.is_rename:
                # A file can be theoretically moved to a location that
                # belongs to a different ChangeLog.  Let user fix it.
                #
                # Since unidiff 0.7.0, path.file == path.target_file[2:],
                # it used to be path.source_file[2:]
                relative_path = get_rel_path_if_prefixed(file.source_file[2:],
                                                         changelog)
                out = append_changelog_line(out, relative_path, 'Move to...')
                new_path = get_rel_path_if_prefixed(file.target_file[2:],
                                                    changelog)
                out += f'\t* {new_path}: ...here.\n'
            elif os.path.basename(file.path) in generated_files:
                out += '\t* %s: Regenerate.\n' % (relative_path)
                append_changelog_line(out, relative_path, 'Regenerate.')
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
                                    # special-case definition in .md files
                                    m2 = md_def_regex.match(line.value)
                                    if extension == '.md' and m2:
                                        fn = m2.group(1)
                                        if fn not in functions:
                                            functions.append(fn)
                                            last_fn = None
                                            success = True

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


def update_copyright(data):
    current_timestamp = datetime.datetime.now().strftime('%Y-%m-%d')
    username = subprocess.check_output('git config user.name', shell=True,
                                       encoding='utf8').strip()
    email = subprocess.check_output('git config user.email', shell=True,
                                    encoding='utf8').strip()

    changelogs = set()
    diff = PatchSet(data)

    for file in diff:
        changelog = os.path.join(find_changelog(file.path), 'ChangeLog')
        if changelog not in changelogs:
            changelogs.add(changelog)
            with open(changelog) as f:
                content = f.read()
            with open(changelog, 'w+') as f:
                f.write(f'{current_timestamp}  {username}  <{email}>\n\n')
                f.write('\tUpdate copyright years.\n\n')
                f.write(content)


def skip_line_in_changelog(line):
    return FIRST_LINE_OF_END_RE.match(line) is None


if __name__ == '__main__':
    extra_args = os.getenv('GCC_MKLOG_ARGS')
    if extra_args:
        sys.argv += json.loads(extra_args)

    parser = argparse.ArgumentParser(description=help_message)
    parser.add_argument('input', nargs='?',
                        help='Patch file (or missing, read standard input)')
    parser.add_argument('-b', '--pr-numbers', action='store',
                        type=lambda arg: arg.split(','), nargs='?',
                        help='Add the specified PRs (comma separated)')
    parser.add_argument('-s', '--no-functions', action='store_true',
                        help='Do not generate function names in ChangeLogs')
    parser.add_argument('-p', '--fill-up-bug-titles', action='store_true',
                        help='Download title of mentioned PRs')
    parser.add_argument('-d', '--directory',
                        help='Root directory where to search for ChangeLog '
                        'files')
    parser.add_argument('-c', '--changelog',
                        help='Append the ChangeLog to a git commit message '
                             'file')
    parser.add_argument('--update-copyright', action='store_true',
                        help='Update copyright in ChangeLog files')
    parser.add_argument('-a', '--append', action='store_true',
                        help='Append the generate ChangeLog to the patch file')
    args = parser.parse_args()
    if args.input == '-':
        args.input = None
    if args.directory:
        root = args.directory

    data = open(args.input, newline='\n') if args.input else sys.stdin
    if args.update_copyright:
        update_copyright(data)
    else:
        output = generate_changelog(data, args.no_functions,
                                    args.fill_up_bug_titles, args.pr_numbers)
        if args.append:
            if (not args.input):
                raise Exception("`-a or --append` option not support standard "
                                "input")
            lines = []
            with open(args.input, 'r', newline='\n') as f:
                # 1 -> not find the possible start of diff log
                # 2 -> find the possible start of diff log
                # 3 -> finish add ChangeLog to the patch file
                maybe_diff_log = 1
                for line in f:
                    if maybe_diff_log == 1 and line == "---\n":
                        maybe_diff_log = 2
                    elif (maybe_diff_log == 2 and
                          re.match(r"\s[^\s]+\s+\|\s+\d+\s[+\-]+\n", line)):
                        lines += [output, "---\n", line]
                        maybe_diff_log = 3
                    else:
                        # the possible start is not the true start.
                        if maybe_diff_log == 2:
                            lines.append("---\n")
                            maybe_diff_log = 1
                        lines.append(line)
            with open(args.input, "w") as f:
                f.writelines(lines)
        elif args.changelog:
            lines = open(args.changelog).read().split('\n')
            start = list(takewhile(skip_line_in_changelog, lines))
            end = lines[len(start):]
            with open(args.changelog, 'w') as f:
                if not start or not start[0]:
                    if len(prs) == 1:
                        # initial commit subject line 'component: [PRnnnnn]'
                        m = prnum_regex.match(prs[0])
                        if m:
                            title = f'{m.group("comp")}: [PR{m.group("num")}]'
                            start.insert(0, title)
                if start:
                    # append empty line
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
