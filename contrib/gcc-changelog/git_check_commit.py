#!/usr/bin/env python3

# Copyright (C) 2020-2025 Free Software Foundation, Inc.
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
# <http://www.gnu.org/licenses/>.

import argparse

from git_repository import parse_git_revisions

def nonzero_uint(value):
    ivalue = int(value)
    if ivalue <= 0:
        raise argparse.ArgumentTypeError('%s is not a non-zero positive integer' % value)
    return ivalue

parser = argparse.ArgumentParser(description='Check git ChangeLog format '
                                 'of a commit')
parser.add_argument('revisions', default='HEAD', nargs='?',
                    help='Git revisions (e.g. hash~5..hash or just hash) - '
                    'if not specified: HEAD')
parser.add_argument('-g', '--git-path', default='.',
                    help='Path to git repository')
parser.add_argument('-p', '--print-changelog', action='store_true',
                    help='Print final changelog entires')
parser.add_argument('-v', '--verbose', action='store_true',
                    help='Print verbose information')
parser.add_argument('-n', '--num-commits', type=nonzero_uint, default=1,
                    help='Number of commits to check (i.e. shorthand for '
                    'hash~N..hash)')
args = parser.parse_args()

if args.num_commits > 1:
    if '..' in args.revisions:
        print('ERR: --num-commits and range of revisions are mutually exclusive')
        exit(1)
    args.revisions = '{0}~{1}..{0}'.format(args.revisions, args.num_commits)

retval = 0
for git_commit in parse_git_revisions(args.git_path, args.revisions):
    res = 'OK' if git_commit.success else 'FAILED'
    print('Checking %s: %s' % (git_commit.original_info.hexsha, res))
    if git_commit.success:
        if args.print_changelog:
            git_commit.print_output()
        if args.verbose and git_commit.warnings:
            for warning in git_commit.warnings:
                print('WARN: %s' % warning)
    else:
        if args.verbose and git_commit.warnings:
            for warning in git_commit.warnings:
                print('WARN: %s' % warning)
        for error in git_commit.errors:
            print('ERR: %s' % error)
            if args.verbose and error.details:
                print(error.details)
        retval = 1

exit(retval)
