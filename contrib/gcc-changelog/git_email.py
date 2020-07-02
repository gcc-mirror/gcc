#!/usr/bin/env python3
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
import sys
from itertools import takewhile

from dateutil.parser import parse

from git_commit import GitCommit, GitInfo

from unidiff import PatchSet

DATE_PREFIX = 'Date: '
FROM_PREFIX = 'From: '


class GitEmail(GitCommit):
    def __init__(self, filename, strict=False):
        self.filename = filename
        diff = PatchSet.from_filename(filename)
        date = None
        author = None

        with open(self.filename, 'r') as f:
            lines = f.read().splitlines()
        lines = list(takewhile(lambda line: line != '---', lines))
        for line in lines:
            if line.startswith(DATE_PREFIX):
                date = parse(line[len(DATE_PREFIX):])
            elif line.startswith(FROM_PREFIX):
                author = GitCommit.format_git_author(line[len(FROM_PREFIX):])
        header = list(takewhile(lambda line: line != '', lines))
        body = lines[len(header) + 1:]

        modified_files = []
        for f in diff:
            # Strip "a/" and "b/" prefixes
            source = f.source_file[2:]
            target = f.target_file[2:]

            if f.is_added_file:
                t = 'A'
            elif f.is_removed_file:
                t = 'D'
            elif f.is_rename:
                # Consider that renamed files are two operations: the deletion
                # of the original name and the addition of the new one.
                modified_files.append((source, 'D'))
                t = 'A'
            else:
                t = 'M'
            modified_files.append((target, t))
        git_info = GitInfo(None, date, author, body, modified_files)
        super().__init__(git_info, strict=strict,
                         commit_to_info_hook=lambda x: None)


# With zero arguments, process every patch file in the ./patches directory.
# With one argument, process the named patch file.
# Patch files must be in 'git format-patch' format.
if __name__ == '__main__':
    if len(sys.argv) == 1:
        allfiles = []
        for root, _dirs, files in os.walk('patches'):
            for f in files:
                full = os.path.join(root, f)
                allfiles.append(full)

        success = 0
        for full in sorted(allfiles):
            email = GitEmail(full, False)
            print(email.filename)
            if email.success:
                success += 1
                print('  OK')
            else:
                for error in email.errors:
                    print('  ERR: %s' % error)

        print()
        print('Successfully parsed: %d/%d' % (success, len(allfiles)))
    else:
        email = GitEmail(sys.argv[1], False)
        if email.success:
            print('OK')
            email.print_output()
        else:
            if not email.info.lines:
                print('Error: patch contains no parsed lines', file=sys.stderr)
            email.print_errors()
            sys.exit(1)
