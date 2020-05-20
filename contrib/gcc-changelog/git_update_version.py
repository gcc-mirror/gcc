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
import datetime
import os

from git import Repo

from git_repository import parse_git_revisions

# TODO: remove sparta suffix
current_timestamp = datetime.datetime.now().strftime('%Y%m%d sparta\n')


def read_timestamp(path):
    return open(path).read()


def prepend_to_changelog_files(repo, folder, git_commit):
    if not git_commit.success:
        for error in git_commit.errors:
            print(error)
        # TODO: add error message
        return
    for entry, output in git_commit.to_changelog_entries(use_commit_ts=True):
        # TODO
        full_path = os.path.join(folder, entry, 'ChangeLog.test')
        print('writting to %s' % full_path)
        if os.path.exists(full_path):
            content = open(full_path).read()
        else:
            content = ''
        with open(full_path, 'w+') as f:
            f.write(output)
            if content:
                f.write('\n\n')
                f.write(content)
        repo.git.add(full_path)


active_refs = ['master', 'releases/gcc-8', 'releases/gcc-9', 'releases/gcc-10']

parser = argparse.ArgumentParser(description='Update DATESTAMP and generate '
                                 'ChangeLog entries')
parser.add_argument('-g', '--git-path', default='.',
                    help='Path to git repository')
args = parser.parse_args()

repo = Repo(args.git_path)
origin = repo.remotes['origin']

for ref in origin.refs:
    assert ref.name.startswith('origin/')
    name = ref.name[len('origin/'):]
    if name in active_refs:
        if name in repo.branches:
            branch = repo.branches[name]
        else:
            branch = repo.create_head(name, ref).set_tracking_branch(ref)
        origin.pull(rebase=True)
        branch.checkout()
        print('=== Working on: %s ===' % branch)
        assert not repo.index.diff(None)
        commit = branch.commit
        commit_count = 1
        while commit:
            if (commit.author.email == 'gccadmin@gcc.gnu.org'
                    and commit.message.strip() == 'Daily bump.'):
                break
            commit = commit.parents[0]
            commit_count += 1

        print('%d revisions since last Daily bump' % commit_count)
        datestamp_path = os.path.join(args.git_path, 'gcc/DATESTAMP')
        if read_timestamp(datestamp_path) != current_timestamp:
            print('DATESTAMP will be changed:')
            # TODO: set strict=True after testing period
            commits = parse_git_revisions(args.git_path, '%s..HEAD'
                                          % commit.hexsha, strict=False)
            for git_commit in reversed(commits):
                prepend_to_changelog_files(repo, args.git_path, git_commit)
            # update timestamp
            with open(datestamp_path, 'w+') as f:
                f.write(current_timestamp)
            repo.git.add(datestamp_path)
            repo.index.commit('Daily bump.')
            # TODO: push the repository
        else:
            print('DATESTAMP unchanged')
