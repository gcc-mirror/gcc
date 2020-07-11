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
    parser = argparse.ArgumentParser(description='Backport a git revision and '
                                     'stash all ChangeLog files.')
    parser.add_argument('revision', help='Revision')
    args = parser.parse_args()

    r = subprocess.run('git cherry-pick -x %s' % args.revision, shell=True)
    if r.returncode == 0:
        cmd = 'git show --name-only --pretty="" -- "*ChangeLog"'
        changelogs = subprocess.check_output(cmd, shell=True, encoding='utf8')
        changelogs = changelogs.strip()
        if changelogs:
            for changelog in changelogs.split('\n'):
                subprocess.check_output('git checkout HEAD~ %s' % changelog,
                                        shell=True)
        subprocess.check_output('git commit --amend --no-edit', shell=True)
    else:
        # 1) remove all ChangeLog files from conflicts
        out = subprocess.check_output('git diff --name-only --diff-filter=U',
                                      shell=True,
                                      encoding='utf8')
        conflicts = out.strip().split('\n')
        changelogs = [c for c in conflicts if c.endswith('ChangeLog')]
        if changelogs:
            cmd = 'git checkout --theirs %s' % '\n'.join(changelogs)
            subprocess.check_output(cmd, shell=True)
        # 2) remove all ChangeLog files from index
        cmd = 'git diff --name-only --diff-filter=M HEAD'
        out = subprocess.check_output(cmd, shell=True, encoding='utf8')
        out = out.strip().split('\n')
        modified = [c for c in out if c.endswith('ChangeLog')]
        for m in modified:
            subprocess.check_output('git reset %s' % m, shell=True)
            subprocess.check_output('git checkout %s' % m, shell=True)

        # try to continue
        if len(conflicts) == len(changelogs):
            cmd = 'git -c core.editor=true cherry-pick --continue'
            subprocess.check_output(cmd, shell=True)
        else:
            print('Please resolve all remaining file conflicts.')
