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

current_timestamp = datetime.datetime.now().strftime('%Y%m%d\n')

# Skip the following commits, they cannot be correctly processed
IGNORED_COMMITS = ('c2be82058fb40f3ae891c68d185ff53e07f14f45')


def read_timestamp(path):
    with open(path) as f:
        return f.read()


def prepend_to_changelog_files(repo, folder, git_commit, add_to_git):
    if not git_commit.success:
        for error in git_commit.errors:
            print(error)
        raise AssertionError()
    for entry, output in git_commit.to_changelog_entries(use_commit_ts=True):
        full_path = os.path.join(folder, entry, 'ChangeLog')
        print('writing to %s' % full_path)
        if os.path.exists(full_path):
            with open(full_path) as f:
                content = f.read()
        else:
            content = ''
        with open(full_path, 'w+') as f:
            f.write(output)
            if content:
                f.write('\n\n')
                f.write(content)
        if add_to_git:
            repo.git.add(full_path)


active_refs = ['master', 'releases/gcc-9', 'releases/gcc-10',
               'releases/gcc-11']

parser = argparse.ArgumentParser(description='Update DATESTAMP and generate '
                                 'ChangeLog entries')
parser.add_argument('-g', '--git-path', default='.',
                    help='Path to git repository')
parser.add_argument('-p', '--push', action='store_true',
                    help='Push updated active branches')
parser.add_argument('-d', '--dry-mode',
                    help='Generate patch for ChangeLog entries and do it'
                         ' even if DATESTAMP is unchanged; folder argument'
                         ' is expected')
parser.add_argument('-c', '--current', action='store_true',
                    help='Modify current branch (--push argument is ignored)')
args = parser.parse_args()

repo = Repo(args.git_path)
origin = repo.remotes['origin']


def update_current_branch(ref_name):
    commit = repo.head.commit
    commit_count = 1
    while commit:
        if (commit.author.email == 'gccadmin@gcc.gnu.org'
                and commit.message.strip() == 'Daily bump.'):
            break
        # We support merge commits but only with 2 parensts
        assert len(commit.parents) <= 2
        commit = commit.parents[-1]
        commit_count += 1

    print('%d revisions since last Daily bump' % commit_count)
    datestamp_path = os.path.join(args.git_path, 'gcc/DATESTAMP')
    if (read_timestamp(datestamp_path) != current_timestamp
            or args.dry_mode or args.current):
        head = repo.head.commit
        # if HEAD is a merge commit, start with second parent
        # (branched that is being merged into the current one)
        assert len(head.parents) <= 2
        if len(head.parents) == 2:
            head = head.parents[1]
        commits = parse_git_revisions(args.git_path, '%s..%s'
                                      % (commit.hexsha, head.hexsha), ref_name)
        commits = [c for c in commits if c.info.hexsha not in IGNORED_COMMITS]
        for git_commit in reversed(commits):
            prepend_to_changelog_files(repo, args.git_path, git_commit,
                                       not args.dry_mode)
        if args.dry_mode:
            diff = repo.git.diff('HEAD')
            patch = os.path.join(args.dry_mode,
                                 branch.name.split('/')[-1] + '.patch')
            with open(patch, 'w+') as f:
                f.write(diff)
            print('branch diff written to %s' % patch)
            repo.git.checkout(force=True)
        else:
            # update timestamp
            print('DATESTAMP will be changed:')
            with open(datestamp_path, 'w+') as f:
                f.write(current_timestamp)
            repo.git.add(datestamp_path)
            if not args.current:
                repo.index.commit('Daily bump.')
                if args.push:
                    repo.git.push('origin', branch)
                    print('branch is pushed')
    else:
        print('DATESTAMP unchanged')


if args.current:
    print('=== Working on the current branch ===', flush=True)
    update_current_branch()
else:
    for ref in origin.refs:
        assert ref.name.startswith('origin/')
        name = ref.name[len('origin/'):]
        if name in active_refs:
            if name in repo.branches:
                branch = repo.branches[name]
            else:
                branch = repo.create_head(name, ref).set_tracking_branch(ref)
            print('=== Working on: %s ===' % branch, flush=True)
            branch.checkout()
            origin.pull(rebase=True)
            print('branch pulled and checked out')
            update_current_branch(name)
            assert not repo.index.diff(None)
            print('branch is done\n', flush=True)
