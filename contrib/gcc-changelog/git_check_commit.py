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

import argparse

from git_repository import parse_git_revisions

parser = argparse.ArgumentParser(description='Check git ChangeLog format '
                                 'of a commit')
parser.add_argument('revisions', default='HEAD', nargs='?',
                    help='Git revisions (e.g. hash~5..hash or just hash) - '
                    'if not specified: HEAD')
parser.add_argument('-g', '--git-path', default='.',
                    help='Path to git repository')
parser.add_argument('-p', '--print-changelog', action='store_true',
                    help='Print final changelog entires')
args = parser.parse_args()

retval = 0
for git_commit in parse_git_revisions(args.git_path, args.revisions):
    res = 'OK' if git_commit.success else 'FAILED'
    print('Checking %s: %s' % (git_commit.original_info.hexsha, res))
    if git_commit.success:
        if args.print_changelog:
            git_commit.print_output()
    else:
        for error in git_commit.errors:
            print('ERR: %s' % error)
        retval = 1

exit(retval)
