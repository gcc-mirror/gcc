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
#
# The script tries to fix commit message where ChangeLog entries
# can point to .cc renamed files.

import argparse
import os
import subprocess
import tempfile

DESCRIPTION = 'Fix up ChangeLog of the current commit.'

script_folder = os.path.dirname(os.path.abspath(__file__))
verify_script = os.path.join(script_folder,
                             'gcc-changelog/git_check_commit.py')


def replace_file_in_changelog(lines, filename, fixed):
    # consider all componenets of a path: gcc/ipa-icf.cc
    while filename:
        for i, line in enumerate(lines):
            if filename in line:
                lines[i] = line.replace(filename, fixed)
                return

        parts = filename.split('/')
        if len(parts) == 1:
            return
        filename = '/'.join(parts[1:])
        fixed = '/'.join(fixed.split('/')[1:])


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=DESCRIPTION)
    args = parser.parse_args()

    # Update commit message if change for a .cc file was taken
    r = subprocess.run(f'{verify_script} HEAD', shell=True, encoding='utf8',
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if r.returncode != 0:
        lines = r.stdout.splitlines()
        cmd = 'git show -s --format=%B'
        commit_message = subprocess.check_output(cmd, shell=True,
                                                 encoding='utf8').strip()
        commit_message = commit_message.splitlines()

        # Parse the following lines:
        # ERR: unchanged file mentioned in a ChangeLog \
        # (did you mean "gcc/ipa-icf.cc"?): "gcc/ipa-icf.c"
        replaced = 0
        for line in lines:
            if ('unchanged file mentioned' in line and
                    'did you mean' in line):
                filename = line.split()[-1].strip('"')
                fixed = line[line.index('did you mean'):]
                fixed = fixed[fixed.index('"') + 1:]
                fixed = fixed[:fixed.index('"')]

                if filename.count('/') == fixed.count('/'):
                    replace_file_in_changelog(commit_message, filename, fixed)
                    replaced += 1

        if replaced:
            with tempfile.NamedTemporaryFile('w', encoding='utf8',
                                             delete=False) as w:
                w.write('\n'.join(commit_message))
                w.close()
                subprocess.check_output(f'git commit --amend -F {w.name}',
                                        shell=True, encoding='utf8')
                os.unlink(w.name)
                print(f'Commit message updated: {replaced} file(s) renamed.')
        else:
            print('Commit message has not been updated.')
