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

import argparse
import subprocess

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Backport a git revision.')
    parser.add_argument('revision', help='Revision')
    args = parser.parse_args()

    subprocess.run('git cherry-pick -x %s' % args.revision, shell=True)
