#!/usr/bin/env python3

# Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

import os
import re
import sys
from itertools import takewhile

from dateutil.parser import parse

from git_commit import GitCommit, GitInfo, decode_path

from unidiff import PatchSet, PatchedFile

DATE_PREFIX = 'Date: '
FROM_PREFIX = 'From: '
SUBJECT_PREFIX = 'Subject: '
subject_patch_regex = re.compile(r'^\[PATCH( \d+/\d+)?\] ')
unidiff_supports_renaming = hasattr(PatchedFile(), 'is_rename')


class GitEmail(GitCommit):
    def __init__(self, filename):
        self.filename = filename
        date = None
        author = None
        subject = ''

        subject_last = False
        with open(self.filename, newline='\n') as f:
            data = f.read()
            diff = PatchSet(data)
            lines = data.splitlines()
        lines = list(takewhile(lambda line: line != '---', lines))
        for line in lines:
            if line.startswith(DATE_PREFIX):
                date = parse(line[len(DATE_PREFIX):])
            elif line.startswith(FROM_PREFIX):
                author = GitCommit.format_git_author(line[len(FROM_PREFIX):])
            elif line.startswith(SUBJECT_PREFIX):
                subject = line[len(SUBJECT_PREFIX):]
                subject_last = True
            elif subject_last and line.startswith(' '):
                subject += line
            elif line == '':
                break
            else:
                subject_last = False

        if subject:
            subject = subject_patch_regex.sub('', subject)
        header = list(takewhile(lambda line: line != '', lines))
        # Note: commit message consists of email subject, empty line, email body
        message = [subject] + lines[len(header):]

        modified_files = []
        for f in diff:
            # Strip "a/" and "b/" prefixes
            source = decode_path(f.source_file)[2:]
            target = decode_path(f.target_file)[2:]

            if f.is_added_file:
                t = 'A'
            elif f.is_removed_file:
                t = 'D'
            elif unidiff_supports_renaming and f.is_rename:
                # Consider that renamed files are two operations: the deletion
                # of the original name and the addition of the new one.
                modified_files.append((source, 'D'))
                t = 'A'
            else:
                t = 'M'
            modified_files.append((target if t != 'D' else source, t))
        git_info = GitInfo(None, date, author, message, modified_files)
        super().__init__(git_info,
                         commit_to_info_hook=lambda x: None)


def show_help():
    print("""usage: git_email.py [--help] [patch file ...]

Check git ChangeLog format of a patch

With zero arguments, process every patch file in the
./patches directory.
With one argument, process the named patch file.

Patch files must be in 'git format-patch' format.""")
    sys.exit(0)


if __name__ == '__main__':
    if len(sys.argv) == 2 and (sys.argv[1] == '-h' or sys.argv[1] == '--help'):
        show_help()

    if len(sys.argv) == 1:
        allfiles = []
        for root, _dirs, files in os.walk('patches'):
            for f in files:
                full = os.path.join(root, f)
                allfiles.append(full)

        success = 0
        for full in sorted(allfiles):
            email = GitEmail(full)
            print(email.filename)
            if email.success:
                success += 1
                print('  OK')
                for warning in email.warnings:
                    print('  WARN: %s' % warning)
            else:
                for error in email.errors:
                    print('  ERR: %s' % error)

        print()
        print('Successfully parsed: %d/%d' % (success, len(allfiles)))
    else:
        email = GitEmail(sys.argv[1])
        if email.success:
            print('OK')
            email.print_output()
            email.print_warnings()
        else:
            if not email.info.lines:
                print('Error: patch contains no parsed lines', file=sys.stderr)
            email.print_errors()
            sys.exit(1)
