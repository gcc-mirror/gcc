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
import os
import subprocess
import tempfile

script_folder = os.path.dirname(os.path.abspath(__file__))
verify_script = os.path.join(script_folder,
                             'gcc-changelog/git_check_commit.py')


def replace_file_in_changelog(lines, filename):
    if not filename.endswith('.cc'):
        return

    # consider all componenets of a path: gcc/ipa-icf.cc
    while filename:
        for i, line in enumerate(lines):
            if filename in line:
                line = line.replace(filename, filename[:-1])
                lines[i] = line
                return
        parts = filename.split('/')
        if len(parts) == 1:
            return
        filename = '/'.join(parts[1:])


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Backport a git revision.')
    parser.add_argument('revision', help='Revision')
    args = parser.parse_args()

    subprocess.run('git cherry-pick -x %s' % args.revision, shell=True)

    # Update commit message if change for a .cc file was taken
    r = subprocess.run(f'{verify_script} HEAD', shell=True, encoding='utf8',
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if r.returncode != 0:
        lines = r.stdout.splitlines()
        cmd = 'git show -s --format=%B'
        commit_message = subprocess.check_output(cmd, shell=True,
                                                 encoding='utf8').strip()
        commit_message = commit_message.splitlines()

        todo = [line for line in lines if 'unchanged file mentioned' in line]
        for item in todo:
            filename = item.split()[-1].strip('"')
            replace_file_in_changelog(commit_message, filename)

        with tempfile.NamedTemporaryFile('w', encoding='utf8',
                                         delete=False) as w:
            w.write('\n'.join(commit_message))
            w.close()
            subprocess.check_output(f'git commit --amend -F {w.name}',
                                    shell=True, encoding='utf8')
            os.unlink(w.name)
            print(f'Commit message updated: {len(todo)} .cc file(s) changed.')
