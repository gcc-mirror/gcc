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
import re

changelog_locations = set([
    'config',
    'contrib',
    'contrib/header-tools',
    'contrib/reghunt',
    'contrib/regression',
    'fixincludes',
    'gcc/ada',
    'gcc/analyzer',
    'gcc/brig',
    'gcc/c',
    'gcc/c-family',
    'gcc',
    'gcc/cp',
    'gcc/d',
    'gcc/fortran',
    'gcc/go',
    'gcc/jit',
    'gcc/lto',
    'gcc/objc',
    'gcc/objcp',
    'gcc/po',
    'gcc/testsuite',
    'gnattools',
    'gotools',
    'include',
    'intl',
    'libada',
    'libatomic',
    'libbacktrace',
    'libcc1',
    'libcpp',
    'libcpp/po',
    'libdecnumber',
    'libffi',
    'libgcc',
    'libgcc/config/avr/libf7',
    'libgcc/config/libbid',
    'libgfortran',
    'libgomp',
    'libhsail-rt',
    'libiberty',
    'libitm',
    'libobjc',
    'liboffloadmic',
    'libphobos',
    'libquadmath',
    'libsanitizer',
    'libssp',
    'libstdc++-v3',
    'libvtv',
    'lto-plugin',
    'maintainer-scripts',
    'zlib'])

bug_components = set([
    'ada',
    'analyzer',
    'boehm-gc',
    'bootstrap',
    'c',
    'c++',
    'd',
    'debug',
    'demangler',
    'driver',
    'fastjar',
    'fortran',
    'gcov-profile',
    'go',
    'hsa',
    'inline-asm',
    'ipa',
    'java',
    'jit',
    'libbacktrace',
    'libf2c',
    'libffi',
    'libfortran',
    'libgcc',
    'libgcj',
    'libgomp',
    'libitm',
    'libobjc',
    'libquadmath',
    'libstdc++',
    'lto',
    'middle-end',
    'modula2',
    'objc',
    'objc++',
    'other',
    'pch',
    'pending',
    'plugins',
    'preprocessor',
    'regression',
    'rtl-optimization',
    'sanitizer',
    'spam',
    'target',
    'testsuite',
    'translation',
    'tree-optimization',
    'web'])

ignored_prefixes = [
    'gcc/d/dmd/',
    'gcc/go/frontend/',
    'libgo/',
    'libphobos/libdruntime',
    'libphobos/src/',
    'libsanitizer/',
    ]

misc_files = [
    'gcc/DATESTAMP',
    'gcc/BASE-VER',
    'gcc/DEV-PHASE'
    ]

author_line_regex = \
        re.compile(r'^(?P<datetime>\d{4}-\d{2}-\d{2})\ {2}(?P<name>.*  <.*>)')
additional_author_regex = re.compile(r'^\t(?P<spaces>\ *)?(?P<name>.*  <.*>)')
changelog_regex = re.compile(r'^([a-z0-9+-/]*)/ChangeLog:?')
pr_regex = re.compile(r'\tPR (?P<component>[a-z+-]+\/)?([0-9]+)$')
star_prefix_regex = re.compile(r'\t\*(?P<spaces>\ *)(?P<content>.*)')

LINE_LIMIT = 100
TAB_WIDTH = 8
CO_AUTHORED_BY_PREFIX = 'co-authored-by: '
CHERRY_PICK_PREFIX = '(cherry picked from commit '
REVIEWED_BY_PREFIX = 'reviewed-by: '
REVIEWED_ON_PREFIX = 'reviewed-on: '
SIGNED_OFF_BY_PREFIX = 'signed-off-by: '

REVIEW_PREFIXES = (REVIEWED_BY_PREFIX, REVIEWED_ON_PREFIX,
                   SIGNED_OFF_BY_PREFIX)


class Error:
    def __init__(self, message, line=None):
        self.message = message
        self.line = line

    def __repr__(self):
        s = self.message
        if self.line:
            s += ':"%s"' % self.line
        return s


class ChangeLogEntry:
    def __init__(self, folder, authors, prs):
        self.folder = folder
        # Python2 has not 'copy' function
        self.author_lines = list(authors)
        self.initial_prs = list(prs)
        self.prs = list(prs)
        self.lines = []

    @property
    def files(self):
        files = []
        for line in self.lines:
            m = star_prefix_regex.match(line)
            if m:
                line = m.group('content')
                if '(' in line:
                    line = line[:line.index('(')]
                if ':' in line:
                    line = line[:line.index(':')]
                for file in line.split(','):
                    file = file.strip()
                    if file:
                        files.append(file)
        return files

    @property
    def datetime(self):
        for author in self.author_lines:
            if author[1]:
                return author[1]
        return None

    @property
    def authors(self):
        return [author_line[0] for author_line in self.author_lines]

    @property
    def is_empty(self):
        return not self.lines and self.prs == self.initial_prs


class GitCommit:
    def __init__(self, hexsha, date, author, body, modified_files,
                 strict=True):
        self.hexsha = hexsha
        self.lines = body
        self.modified_files = modified_files
        self.message = None
        self.changes = None
        self.changelog_entries = []
        self.errors = []
        self.date = date
        self.author = author
        self.top_level_authors = []
        self.co_authors = []
        self.top_level_prs = []

        project_files = [f for f in self.modified_files
                         if self.is_changelog_filename(f[0])
                         or f[0] in misc_files]
        if len(project_files) == len(self.modified_files):
            # All modified files are only MISC files
            return
        elif project_files and strict:
            self.errors.append(Error('ChangeLog, DATESTAMP, BASE-VER and '
                                     'DEV-PHASE updates should be done '
                                     'separately from normal commits'))
            return

        self.parse_lines()
        if self.changes:
            self.parse_changelog()
            self.deduce_changelog_locations()
            if not self.errors:
                self.check_mentioned_files()
                self.check_for_correct_changelog()

    @property
    def success(self):
        return not self.errors

    @property
    def new_files(self):
        return [x[0] for x in self.modified_files if x[1] == 'A']

    @classmethod
    def is_changelog_filename(cls, path):
        return path.endswith('/ChangeLog') or path == 'ChangeLog'

    @classmethod
    def find_changelog_location(cls, name):
        if name.startswith('\t'):
            name = name[1:]
        if name.endswith(':'):
            name = name[:-1]
        if name.endswith('/'):
            name = name[:-1]
        return name if name in changelog_locations else None

    @classmethod
    def format_git_author(cls, author):
        assert '<' in author
        return author.replace('<', ' <')

    @classmethod
    def parse_git_name_status(cls, string):
        modified_files = []
        for entry in string.split('\n'):
            parts = entry.split('\t')
            t = parts[0]
            if t == 'A' or t == 'D' or t == 'M':
                modified_files.append((parts[1], t))
            elif t == 'R':
                modified_files.append((parts[1], 'D'))
                modified_files.append((parts[2], 'A'))
        return modified_files

    def parse_lines(self):
        body = self.lines

        for i, b in enumerate(body):
            if not b:
                continue
            if (changelog_regex.match(b) or self.find_changelog_location(b)
                    or star_prefix_regex.match(b) or pr_regex.match(b)
                    or author_line_regex.match(b)):
                self.changes = body[i:]
                return
        self.errors.append(Error('cannot find a ChangeLog location in '
                                 'message'))

    def parse_changelog(self):
        last_entry = None
        will_deduce = False
        for line in self.changes:
            if not line:
                if last_entry and will_deduce:
                    last_entry = None
                continue
            if line != line.rstrip():
                self.errors.append(Error('trailing whitespace', line))
            if len(line.replace('\t', ' ' * TAB_WIDTH)) > LINE_LIMIT:
                self.errors.append(Error('line limit exceeds %d characters'
                                         % LINE_LIMIT, line))
            m = changelog_regex.match(line)
            if m:
                last_entry = ChangeLogEntry(m.group(1), self.top_level_authors,
                                            self.top_level_prs)
                self.changelog_entries.append(last_entry)
            elif self.find_changelog_location(line):
                last_entry = ChangeLogEntry(self.find_changelog_location(line),
                                            self.top_level_authors,
                                            self.top_level_prs)
                self.changelog_entries.append(last_entry)
            else:
                author_tuple = None
                pr_line = None
                if author_line_regex.match(line):
                    m = author_line_regex.match(line)
                    author_tuple = (m.group('name'), m.group('datetime'))
                elif additional_author_regex.match(line):
                    m = additional_author_regex.match(line)
                    if len(m.group('spaces')) != 4:
                        msg = 'additional author must prepend with tab ' \
                              'and 4 spaces'
                        self.errors.append(Error(msg, line))
                    else:
                        author_tuple = (m.group('name'), None)
                elif pr_regex.match(line):
                    component = pr_regex.match(line).group('component')
                    if not component:
                        self.errors.append(Error('missing PR component', line))
                        continue
                    elif not component[:-1] in bug_components:
                        self.errors.append(Error('invalid PR component', line))
                        continue
                    else:
                        pr_line = line.lstrip()

                lowered_line = line.lower()
                if lowered_line.startswith(CO_AUTHORED_BY_PREFIX):
                    name = line[len(CO_AUTHORED_BY_PREFIX):]
                    author = self.format_git_author(name)
                    self.co_authors.append(author)
                    continue
                elif lowered_line.startswith(REVIEW_PREFIXES):
                    continue
                elif line.startswith(CHERRY_PICK_PREFIX):
                    continue

                # ChangeLog name will be deduced later
                if not last_entry:
                    if author_tuple:
                        self.top_level_authors.append(author_tuple)
                        continue
                    elif pr_line:
                        # append to top_level_prs only when we haven't met
                        # a ChangeLog entry
                        if (pr_line not in self.top_level_prs
                                and not self.changelog_entries):
                            self.top_level_prs.append(pr_line)
                        continue
                    else:
                        last_entry = ChangeLogEntry(None,
                                                    self.top_level_authors,
                                                    self.top_level_prs)
                        self.changelog_entries.append(last_entry)
                        will_deduce = True
                elif author_tuple:
                    last_entry.author_lines.append(author_tuple)
                    continue

                if not line.startswith('\t'):
                    err = Error('line should start with a tab', line)
                    self.errors.append(err)
                elif pr_line:
                    last_entry.prs.append(pr_line)
                else:
                    m = star_prefix_regex.match(line)
                    if m:
                        if len(m.group('spaces')) != 1:
                            err = Error('one space should follow asterisk',
                                        line)
                            self.errors.append(err)
                        else:
                            last_entry.lines.append(line)
                    else:
                        if last_entry.is_empty:
                            msg = 'first line should start with a tab, ' \
                                  'asterisk and space'
                            self.errors.append(Error(msg, line))
                        else:
                            last_entry.lines.append(line)

    def get_file_changelog_location(self, changelog_file):
        for file in self.modified_files:
            if file[0] == changelog_file:
                # root ChangeLog file
                return ''
            index = file[0].find('/' + changelog_file)
            if index != -1:
                return file[0][:index]
        return None

    def deduce_changelog_locations(self):
        for entry in self.changelog_entries:
            if not entry.folder:
                changelog = None
                for file in entry.files:
                    location = self.get_file_changelog_location(file)
                    if (location == ''
                       or (location and location in changelog_locations)):
                        if changelog and changelog != location:
                            msg = 'could not deduce ChangeLog file, ' \
                                  'not unique location'
                            self.errors.append(Error(msg))
                            return
                        changelog = location
                if changelog is not None:
                    entry.folder = changelog
                else:
                    msg = 'could not deduce ChangeLog file'
                    self.errors.append(Error(msg))

    @classmethod
    def in_ignored_location(cls, path):
        for ignored in ignored_prefixes:
            if path.startswith(ignored):
                return True
        return False

    @classmethod
    def get_changelog_by_path(cls, path):
        components = path.split('/')
        while components:
            if '/'.join(components) in changelog_locations:
                break
            components = components[:-1]
        return '/'.join(components)

    def check_mentioned_files(self):
        folder_count = len([x.folder for x in self.changelog_entries])
        assert folder_count == len(self.changelog_entries)

        mentioned_files = set()
        for entry in self.changelog_entries:
            if not entry.files:
                msg = 'ChangeLog must contain a file entry'
                self.errors.append(Error(msg, entry.folder))
            assert not entry.folder.endswith('/')
            for file in entry.files:
                if not self.is_changelog_filename(file):
                    mentioned_files.add(os.path.join(entry.folder, file))

        cand = [x[0] for x in self.modified_files
                if not self.is_changelog_filename(x[0])]
        changed_files = set(cand)
        for file in sorted(mentioned_files - changed_files):
            self.errors.append(Error('file not changed in a patch', file))
        for file in sorted(changed_files - mentioned_files):
            if not self.in_ignored_location(file):
                if file in self.new_files:
                    changelog_location = self.get_changelog_by_path(file)
                    # Python2: we cannot use next(filter(...))
                    entries = filter(lambda x: x.folder == changelog_location,
                                     self.changelog_entries)
                    entries = list(entries)
                    entry = entries[0] if entries else None
                    if not entry:
                        prs = self.top_level_prs
                        if not prs:
                            # if all ChangeLog entries have identical PRs
                            # then use them
                            prs = self.changelog_entries[0].prs
                            for entry in self.changelog_entries:
                                if entry.prs != prs:
                                    prs = []
                                    break
                        entry = ChangeLogEntry(changelog_location,
                                               self.top_level_authors,
                                               prs)
                        self.changelog_entries.append(entry)
                    # strip prefix of the file
                    assert file.startswith(entry.folder)
                    file = file[len(entry.folder):].lstrip('/')
                    entry.lines.append('\t* %s: New file.' % file)
                else:
                    msg = 'changed file not mentioned in a ChangeLog'
                    self.errors.append(Error(msg, file))

    def check_for_correct_changelog(self):
        for entry in self.changelog_entries:
            for file in entry.files:
                full_path = os.path.join(entry.folder, file)
                changelog_location = self.get_changelog_by_path(full_path)
                if changelog_location != entry.folder:
                    msg = 'wrong ChangeLog location "%s", should be "%s"'
                    err = Error(msg % (entry.folder, changelog_location), file)
                    self.errors.append(err)

    def to_changelog_entries(self, use_commit_ts=False):
        for entry in self.changelog_entries:
            output = ''
            timestamp = entry.datetime
            if not timestamp or use_commit_ts:
                timestamp = self.date.strftime('%Y-%m-%d')
            authors = entry.authors if entry.authors else [self.author]
            # add Co-Authored-By authors to all ChangeLog entries
            for author in self.co_authors:
                if author not in authors:
                    authors.append(author)

            for i, author in enumerate(authors):
                if i == 0:
                    output += '%s  %s\n' % (timestamp, author)
                else:
                    output += '\t    %s\n' % author
            output += '\n'
            for pr in entry.prs:
                output += '\t%s\n' % pr
            for line in entry.lines:
                output += line + '\n'
            yield (entry.folder, output.rstrip())

    def print_output(self):
        for entry, output in self.to_changelog_entries():
            print('------ %s/ChangeLog ------ ' % entry)
            print(output)

    def print_errors(self):
        print('Errors:')
        for error in self.errors:
            print(error)
