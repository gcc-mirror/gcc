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

import difflib
import os
import re
import sys

default_changelog_locations = {
    'c++tools',
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
    'libcody',
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
    'zlib'}

bug_components = {
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
    'web'}

ignored_prefixes = {
    'gcc/d/dmd/',
    'gcc/go/gofrontend/',
    'gcc/testsuite/gdc.test/',
    'gcc/testsuite/go.test/test/',
    'libgo/',
    'libphobos/libdruntime/',
    'libphobos/src/',
    'libsanitizer/',
    }

wildcard_prefixes = {
    'gcc/testsuite/',
    'libstdc++-v3/doc/html/',
    'libstdc++-v3/testsuite/'
    }

misc_files = {
    'gcc/DATESTAMP',
    'gcc/BASE-VER',
    'gcc/DEV-PHASE'
    }

author_line_regex = \
        re.compile(r'^(?P<datetime>\d{4}-\d{2}-\d{2})\ {2}(?P<name>.*  <.*>)')
additional_author_regex = re.compile(r'^\t(?P<spaces>\ *)?(?P<name>.*  <.*>)')
changelog_regex = re.compile(r'^(?:[fF]or +)?([a-z0-9+-/]*)ChangeLog:?')
pr_regex = re.compile(r'\tPR (?P<component>[a-z+-]+\/)?([0-9]+)$')
dr_regex = re.compile(r'\tDR ([0-9]+)$')
star_prefix_regex = re.compile(r'\t\*(?P<spaces>\ *)(?P<content>.*)')
end_of_location_regex = re.compile(r'[\[<(:]')
item_empty_regex = re.compile(r'\t(\* \S+ )?\(\S+\):\s*$')
item_parenthesis_regex = re.compile(r'\t(\*|\(\S+\):)')
revert_regex = re.compile(r'This reverts commit (?P<hash>\w+).$')
cherry_pick_regex = re.compile(r'cherry picked from commit (?P<hash>\w+)')

LINE_LIMIT = 100
TAB_WIDTH = 8
CO_AUTHORED_BY_PREFIX = 'co-authored-by: '

REVIEW_PREFIXES = ('reviewed-by: ', 'reviewed-on: ', 'signed-off-by: ',
                   'acked-by: ', 'tested-by: ', 'reported-by: ',
                   'suggested-by: ')
DATE_FORMAT = '%Y-%m-%d'


def decode_path(path):
    # When core.quotepath is true (default value), utf8 chars are encoded like:
    # "b/ko\304\215ka.txt"
    #
    # The upstream bug is fixed:
    # https://github.com/gitpython-developers/GitPython/issues/1099
    #
    # but we still need a workaround for older versions of the library.
    # Please take a look at the explanation of the transformation:
    # https://stackoverflow.com/questions/990169/how-do-convert-unicode-escape-sequences-to-unicode-characters-in-a-python-string

    if path.startswith('"') and path.endswith('"'):
        return (path.strip('"').encode('utf8').decode('unicode-escape')
                .encode('latin-1').decode('utf8'))
    else:
        return path


class Error:
    def __init__(self, message, line=None):
        self.message = message
        self.line = line

    def __repr__(self):
        s = self.message
        if self.line:
            s += ': "%s"' % self.line
        return s


class ChangeLogEntry:
    def __init__(self, folder, authors, prs):
        self.folder = folder
        # The 'list.copy()' function is not available before Python 3.3
        self.author_lines = list(authors)
        self.initial_prs = list(prs)
        self.prs = list(prs)
        self.lines = []
        self.files = []
        self.file_patterns = []
        self.opened_parentheses = 0

    def parse_file_names(self):
        # Whether the content currently processed is between a star prefix the
        # end of the file list: a colon or an open paren.
        in_location = False

        for line in self.lines:
            # If this line matches the star prefix, start the location
            # processing on the information that follows the star.
            # Note that we need to skip macro names that can be in form of:
            #
            # * config/i386/i386.md (*fix_trunc<mode>_i387_1,
            # *add<mode>3_ne, *add<mode>3_eq_0, *add<mode>3_ne_0,
            # *fist<mode>2_<rounding>_1, *<code><mode>3_1):
            #
            m = star_prefix_regex.match(line)
            if m and len(m.group('spaces')) == 1:
                in_location = True
                line = m.group('content')

            if in_location:
                # Strip everything that is not a filename in "line":
                # entities "(NAME)", cases "<PATTERN>", conditions
                # "[COND]", entry text (the colon, if present, and
                # anything that follows it).
                m = end_of_location_regex.search(line)
                if m:
                    line = line[:m.start()]
                    in_location = False

                # At this point, all that's left is a list of filenames
                # separated by commas and whitespaces.
                for file in line.split(','):
                    file = file.strip()
                    if file:
                        if file.endswith('*'):
                            self.file_patterns.append(file[:-1])
                        else:
                            self.files.append(file)

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

    def contains_author(self, author):
        for author_lines in self.author_lines:
            if author_lines[0] == author:
                return True
        return False


class GitInfo:
    def __init__(self, hexsha, date, author, lines, modified_files):
        self.hexsha = hexsha
        self.date = date
        self.author = author
        self.lines = lines
        self.modified_files = modified_files


class GitCommit:
    def __init__(self, info, commit_to_info_hook=None, ref_name=None):
        self.original_info = info
        self.info = info
        self.message = None
        self.changes = None
        self.changelog_entries = []
        self.errors = []
        self.top_level_authors = []
        self.co_authors = []
        self.top_level_prs = []
        self.cherry_pick_commit = None
        self.revert_commit = None
        self.commit_to_info_hook = commit_to_info_hook
        self.init_changelog_locations(ref_name)

        # Skip Update copyright years commits
        if self.info.lines and self.info.lines[0] == 'Update copyright years.':
            return

        # Identify first if the commit is a Revert commit
        for line in self.info.lines:
            m = revert_regex.match(line)
            if m:
                self.revert_commit = m.group('hash')
                break
        if self.revert_commit:
            self.info = self.commit_to_info_hook(self.revert_commit)

        # Allow complete deletion of ChangeLog files in a commit
        project_files = [f for f in self.info.modified_files
                         if (self.is_changelog_filename(f[0], allow_suffix=True) and f[1] != 'D')
                         or f[0] in misc_files]
        ignored_files = [f for f in self.info.modified_files
                         if self.in_ignored_location(f[0])]
        if len(project_files) == len(self.info.modified_files):
            # All modified files are only MISC files
            return
        elif project_files:
            err = 'ChangeLog, DATESTAMP, BASE-VER and DEV-PHASE updates ' \
                  'should be done separately from normal commits\n' \
                  '(note: ChangeLog entries will be automatically ' \
                  'added by a cron job)'
            self.errors.append(Error(err))
            return

        all_are_ignored = (len(project_files) + len(ignored_files)
                           == len(self.info.modified_files))
        self.parse_lines(all_are_ignored)
        if self.changes:
            self.parse_changelog()
            self.parse_file_names()
            self.check_for_empty_description()
            self.check_for_broken_parentheses()
            self.deduce_changelog_locations()
            self.check_file_patterns()
            if not self.errors:
                self.check_mentioned_files()
                self.check_for_correct_changelog()

    @property
    def success(self):
        return not self.errors

    @property
    def new_files(self):
        return [x[0] for x in self.info.modified_files if x[1] == 'A']

    @classmethod
    def is_changelog_filename(cls, path, allow_suffix=False):
        basename = os.path.basename(path)
        if basename == 'ChangeLog':
            return True
        elif allow_suffix and basename.startswith('ChangeLog'):
            return True
        else:
            return False

    def find_changelog_location(self, name):
        if name.startswith('\t'):
            name = name[1:]
        if name.endswith(':'):
            name = name[:-1]
        if name.endswith('/'):
            name = name[:-1]
        return name if name in self.changelog_locations else None

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
            elif t.startswith('R'):
                modified_files.append((parts[1], 'D'))
                modified_files.append((parts[2], 'A'))
        return modified_files

    def init_changelog_locations(self, ref_name):
        self.changelog_locations = list(default_changelog_locations)
        if ref_name:
            version = sys.maxsize
            if 'releases/gcc-' in ref_name:
                version = int(ref_name.split('-')[-1])
            if version >= 12:
                # HSA and BRIG were removed in GCC 12
                self.changelog_locations.remove('gcc/brig')
                self.changelog_locations.remove('libhsail-rt')

    def parse_lines(self, all_are_ignored):
        body = self.info.lines

        for i, b in enumerate(body):
            if not b:
                continue
            if (changelog_regex.match(b) or self.find_changelog_location(b)
                    or star_prefix_regex.match(b) or pr_regex.match(b)
                    or dr_regex.match(b) or author_line_regex.match(b)
                    or b.lower().startswith(CO_AUTHORED_BY_PREFIX)):
                self.changes = body[i:]
                return
        if not all_are_ignored:
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
                # support long filenames
                if not line.startswith('\t* ') or not line.endswith(':') or ' ' in line[3:-1]:
                    self.errors.append(Error('line exceeds %d character limit'
                                             % LINE_LIMIT, line))
            m = changelog_regex.match(line)
            if m:
                last_entry = ChangeLogEntry(m.group(1).rstrip('/'),
                                            self.top_level_authors,
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
                        msg = 'additional author must be indented with '\
                              'one tab and four spaces'
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
                elif dr_regex.match(line):
                    pr_line = line.lstrip()

                lowered_line = line.lower()
                if lowered_line.startswith(CO_AUTHORED_BY_PREFIX):
                    name = line[len(CO_AUTHORED_BY_PREFIX):]
                    author = self.format_git_author(name)
                    self.co_authors.append(author)
                    continue
                elif lowered_line.startswith(REVIEW_PREFIXES):
                    continue
                else:
                    m = cherry_pick_regex.search(line)
                    if m:
                        commit = m.group('hash')
                        if self.cherry_pick_commit:
                            msg = 'multiple cherry pick lines'
                            self.errors.append(Error(msg, line))
                        else:
                            self.cherry_pick_commit = commit
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
                    if not last_entry.contains_author(author_tuple[0]):
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
                        if (len(m.group('spaces')) != 1 and
                                last_entry.opened_parentheses == 0):
                            msg = 'one space should follow asterisk'
                            self.errors.append(Error(msg, line))
                        else:
                            content = m.group('content')
                            parts = content.split(':')
                            if len(parts) > 1:
                                for needle in ('()', '[]', '<>'):
                                    if ' ' + needle in parts[0]:
                                        msg = f'empty group "{needle}" found'
                                        self.errors.append(Error(msg, line))
                            last_entry.lines.append(line)
                            self.process_parentheses(last_entry, line)
                    else:
                        if last_entry.is_empty:
                            msg = 'first line should start with a tab, ' \
                                  'an asterisk and a space'
                            self.errors.append(Error(msg, line))
                        else:
                            last_entry.lines.append(line)
                            self.process_parentheses(last_entry, line)

    def process_parentheses(self, last_entry, line):
        for c in line:
            if c == '(':
                last_entry.opened_parentheses += 1
            elif c == ')':
                if last_entry.opened_parentheses == 0:
                    msg = 'bad wrapping of parenthesis'
                    self.errors.append(Error(msg, line))
                else:
                    last_entry.opened_parentheses -= 1

    def parse_file_names(self):
        for entry in self.changelog_entries:
            entry.parse_file_names()

    def check_file_patterns(self):
        for entry in self.changelog_entries:
            for pattern in entry.file_patterns:
                name = os.path.join(entry.folder, pattern)
                if not [name.startswith(pr) for pr in wildcard_prefixes]:
                    msg = 'unsupported wildcard prefix'
                    self.errors.append(Error(msg, name))

    def check_for_empty_description(self):
        for entry in self.changelog_entries:
            for i, line in enumerate(entry.lines):
                if (item_empty_regex.match(line) and
                    (i == len(entry.lines) - 1
                     or not entry.lines[i+1].strip()
                     or item_parenthesis_regex.match(entry.lines[i+1]))):
                    msg = 'missing description of a change'
                    self.errors.append(Error(msg, line))

    def check_for_broken_parentheses(self):
        for entry in self.changelog_entries:
            if entry.opened_parentheses != 0:
                msg = 'bad parentheses wrapping'
                self.errors.append(Error(msg, entry.lines[0]))

    def get_file_changelog_location(self, changelog_file):
        for file in self.info.modified_files:
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
                       or (location and location in self.changelog_locations)):
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

    def get_changelog_by_path(self, path):
        components = path.split('/')
        while components:
            if '/'.join(components) in self.changelog_locations:
                break
            components = components[:-1]
        return '/'.join(components)

    def check_mentioned_files(self):
        folder_count = len([x.folder for x in self.changelog_entries])
        assert folder_count == len(self.changelog_entries)

        mentioned_files = set()
        mentioned_patterns = []
        used_patterns = set()
        for entry in self.changelog_entries:
            if not entry.files and not entry.file_patterns:
                msg = 'no files mentioned for ChangeLog in directory'
                self.errors.append(Error(msg, entry.folder))
            assert not entry.folder.endswith('/')
            for file in entry.files:
                if not self.is_changelog_filename(file):
                    item = os.path.join(entry.folder, file)
                    if item in mentioned_files:
                        msg = 'same file specified multiple times'
                        self.errors.append(Error(msg, file))
                    else:
                        mentioned_files.add(item)
            for pattern in entry.file_patterns:
                mentioned_patterns.append(os.path.join(entry.folder, pattern))

        cand = [x[0] for x in self.info.modified_files
                if not self.is_changelog_filename(x[0])]
        changed_files = set(cand)
        for file in sorted(mentioned_files - changed_files):
            msg = 'unchanged file mentioned in a ChangeLog'
            candidates = difflib.get_close_matches(file, changed_files, 1)
            if candidates:
                msg += f' (did you mean "{candidates[0]}"?)'
            self.errors.append(Error(msg, file))
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
                    entry.files.append(file)
                else:
                    used_pattern = [p for p in mentioned_patterns
                                    if file.startswith(p)]
                    used_pattern = used_pattern[0] if used_pattern else None
                    if used_pattern:
                        used_patterns.add(used_pattern)
                    else:
                        msg = 'changed file not mentioned in a ChangeLog'
                        self.errors.append(Error(msg, file))

        for pattern in mentioned_patterns:
            if pattern not in used_patterns:
                error = "pattern doesn't match any changed files"
                self.errors.append(Error(error, pattern))

    def check_for_correct_changelog(self):
        for entry in self.changelog_entries:
            for file in entry.files:
                full_path = os.path.join(entry.folder, file)
                changelog_location = self.get_changelog_by_path(full_path)
                if changelog_location != entry.folder:
                    msg = 'wrong ChangeLog location "%s", should be "%s"'
                    err = Error(msg % (entry.folder, changelog_location), file)
                    self.errors.append(err)

    @classmethod
    def format_authors_in_changelog(cls, authors, timestamp, prefix=''):
        output = ''
        for i, author in enumerate(authors):
            if i == 0:
                output += '%s%s  %s\n' % (prefix, timestamp, author)
            else:
                output += '%s\t    %s\n' % (prefix, author)
        output += '\n'
        return output

    def to_changelog_entries(self, use_commit_ts=False):
        current_timestamp = self.info.date.strftime(DATE_FORMAT)
        for entry in self.changelog_entries:
            output = ''
            timestamp = entry.datetime
            if self.revert_commit:
                timestamp = current_timestamp
                orig_date = self.original_info.date
                current_timestamp = orig_date.strftime(DATE_FORMAT)
            elif self.cherry_pick_commit:
                info = self.commit_to_info_hook(self.cherry_pick_commit)
                # it can happen that it is a cherry-pick for a different
                # repository
                if info:
                    timestamp = info.date.strftime(DATE_FORMAT)
                else:
                    timestamp = current_timestamp
            elif not timestamp or use_commit_ts:
                timestamp = current_timestamp
            authors = entry.authors if entry.authors else [self.info.author]
            # add Co-Authored-By authors to all ChangeLog entries
            for author in self.co_authors:
                if author not in authors:
                    authors.append(author)

            if self.cherry_pick_commit or self.revert_commit:
                original_author = self.original_info.author
                output += self.format_authors_in_changelog([original_author],
                                                           current_timestamp)
                if self.revert_commit:
                    output += '\tRevert:\n'
                else:
                    output += '\tBackported from master:\n'
                output += self.format_authors_in_changelog(authors,
                                                           timestamp, '\t')
            else:
                output += self.format_authors_in_changelog(authors, timestamp)
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
