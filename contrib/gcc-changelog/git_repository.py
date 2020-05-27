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

from datetime import datetime

try:
    from git import Repo
except ImportError:
    print('Cannot import GitPython package, please install the package:')
    print('  Fedora, openSUSE: python3-GitPython')
    print('  Debian, Ubuntu: python3-git')
    exit(1)

from git_commit import GitCommit


def parse_git_revisions(repo_path, revisions, strict=False):
    repo = Repo(repo_path)

    parsed_commits = []
    if '..' in revisions:
        commits = list(repo.iter_commits(revisions))
    else:
        commits = [repo.commit(revisions)]

    for commit in commits:
        diff = repo.commit(commit.hexsha + '~').diff(commit.hexsha)

        modified_files = []
        for file in diff:
            if file.new_file:
                t = 'A'
            elif file.deleted_file:
                t = 'D'
            elif file.renamed_file:
                # Consider that renamed files are two operations: the deletion
                # of the original name and the addition of the new one.
                modified_files.append((file.a_path, 'D'))
                t = 'A'
            else:
                t = 'M'
            modified_files.append((file.b_path, t))

        date = datetime.utcfromtimestamp(commit.committed_date)
        author = '%s  <%s>' % (commit.author.name, commit.author.email)
        git_commit = GitCommit(commit.hexsha, date, author,
                               commit.message.split('\n'), modified_files,
                               strict=strict)
        parsed_commits.append(git_commit)
    return parsed_commits
