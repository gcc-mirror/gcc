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
import tempfile
import unittest

from git_commit import GitCommit

from git_email import GitEmail

import unidiff

script_path = os.path.dirname(os.path.realpath(__file__))

unidiff_supports_renaming = hasattr(unidiff.PatchedFile(), 'is_rename')


NAME_STATUS1 = """
M	gcc/ada/impunit.adb'
R097	gcc/ada/libgnat/s-atopar.adb	gcc/ada/libgnat/s-aoinar.adb
"""


class TestGccChangelog(unittest.TestCase):
    def setUp(self):
        self.patches = {}
        self.temps = []

        filename = None
        patch_lines = []
        with open(os.path.join(script_path, 'test_patches.txt')) as f:
            lines = f.read()
        for line in lines.split('\n'):
            if line.startswith('==='):
                if patch_lines:
                    self.patches[filename] = patch_lines
                filename = line.split(' ')[1]
                patch_lines = []
            else:
                patch_lines.append(line)
        if patch_lines:
            self.patches[filename] = patch_lines

    def tearDown(self):
        for t in self.temps:
            assert t.endswith('.patch')
            os.remove(t)

    def get_git_email(self, filename):
        with tempfile.NamedTemporaryFile(mode='w+', suffix='.patch',
                                         delete=False) as f:
            f.write('\n'.join(self.patches[filename]))
            self.temps.append(f.name)
        return GitEmail(f.name)

    def from_patch_glob(self, name):
        files = [f for f in self.patches.keys() if f.startswith(name)]
        assert len(files) == 1
        return self.get_git_email(files[0])

    def test_simple_patch_format(self):
        email = self.get_git_email('0577-aarch64-Add-an-and.patch')
        assert not email.errors
        assert len(email.changelog_entries) == 2
        entry = email.changelog_entries[0]
        assert (entry.author_lines ==
                [('Richard Sandiford  <richard.sandiford@arm.com>',
                  '2020-02-06')])
        assert len(entry.authors) == 1
        assert (entry.authors[0]
                == 'Richard Sandiford  <richard.sandiford@arm.com>')
        assert entry.folder == 'gcc'
        assert entry.prs == ['PR target/87763']
        assert len(entry.files) == 3
        assert entry.files[0] == 'config/aarch64/aarch64-protos.h'

    def test_daily_bump(self):
        email = self.get_git_email('0085-Daily-bump.patch')
        assert not email.errors
        assert not email.changelog_entries

    def test_deduce_changelog_entries(self):
        email = self.from_patch_glob('0040')
        assert len(email.changelog_entries) == 2
        assert email.changelog_entries[0].folder == 'gcc/cp'
        assert email.changelog_entries[0].prs == ['PR c++/90916']
        assert email.changelog_entries[0].files == ['pt.c']
        # this one is added automatically
        assert email.changelog_entries[1].folder == 'gcc/testsuite'

    def test_only_changelog_updated(self):
        email = self.from_patch_glob('0129')
        assert not email.errors
        assert not email.changelog_entries

    def test_wrong_mentioned_filename(self):
        email = self.from_patch_glob('0096')
        assert email.errors
        err = email.errors[0]
        assert err.message == 'unchanged file mentioned in a ChangeLog (did ' \
            'you mean "gcc/testsuite/gcc.target/aarch64/' \
            'advsimd-intrinsics/vdot-3-1.c"?)'
        assert err.line == 'gcc/testsuite/gcc.target/aarch64/' \
                           'advsimd-intrinsics/vdot-compile-3-1.c'

    def test_missing_tab(self):
        email = self.from_patch_glob('0031')
        assert len(email.errors) == 2
        err = email.errors[0]
        assert err.message == 'line should start with a tab'
        assert err.line == '    * cfgloopanal.c (average_num_loop_insns): ' \
                           'Free bbs when early'

    def test_leading_changelog_format(self):
        email = self.from_patch_glob('0184')
        assert len(email.errors) == 4
        assert email.errors[0].line == 'gcc/c-family/c-cppbuiltins.c'
        assert email.errors[2].line == 'gcc/c-family/c-cppbuiltin.c'

    def test_cannot_deduce_no_blank_line(self):
        email = self.from_patch_glob('0334')
        assert len(email.errors) == 1
        assert len(email.changelog_entries) == 1
        assert email.changelog_entries[0].folder is None

    def test_author_lines(self):
        email = self.from_patch_glob('0814')
        assert not email.errors
        assert (email.changelog_entries[0].author_lines ==
                [('Martin Jambor  <mjambor@suse.cz>', '2020-02-19')])

    def test_multiple_authors_and_prs(self):
        email = self.from_patch_glob('0735')
        assert len(email.changelog_entries) == 1
        entry = email.changelog_entries[0]
        assert len(entry.author_lines) == 2
        assert len(entry.authors) == 2
        assert (entry.author_lines[1] ==
                ('Bernd Edlinger  <bernd.edlinger@hotmail.de>', None))

    def test_multiple_prs(self):
        email = self.from_patch_glob('1699')
        assert len(email.changelog_entries) == 2
        assert len(email.changelog_entries[0].prs) == 2

    def test_missing_PR_component(self):
        email = self.from_patch_glob('0735')
        assert len(email.errors) == 1
        assert email.errors[0].message == 'missing PR component'

    def test_invalid_PR_component(self):
        email = self.from_patch_glob('0198')
        assert len(email.errors) == 1
        assert email.errors[0].message == 'invalid PR component'

    def test_additional_author_list(self):
        email = self.from_patch_glob('0342')
        msg = 'additional author must be indented ' \
              'with one tab and four spaces'
        assert email.errors[1].message == msg

    def test_trailing_whitespaces(self):
        email = self.get_git_email('trailing-whitespaces.patch')
        assert len(email.errors) == 3

    def test_space_after_asterisk(self):
        email = self.from_patch_glob('1999')
        assert len(email.errors) == 1
        assert email.errors[0].message == 'one space should follow asterisk'

    def test_long_lines(self):
        email = self.get_git_email('long-lines.patch')
        assert len(email.errors) == 1
        assert email.errors[0].message == 'line exceeds 100 character limit'

    def test_new_files(self):
        email = self.from_patch_glob('0030')
        assert not email.errors

    def test_wrong_changelog_location(self):
        email = self.from_patch_glob('0043')
        assert len(email.errors) == 2
        assert (email.errors[0].message ==
                'wrong ChangeLog location "gcc", should be "gcc/testsuite"')

    def test_single_author_name(self):
        email = self.from_patch_glob('1975')
        assert len(email.changelog_entries) == 2
        assert len(email.changelog_entries[0].author_lines) == 1
        assert len(email.changelog_entries[1].author_lines) == 1

    def test_bad_first_line(self):
        email = self.from_patch_glob('0413')
        assert len(email.errors) == 1

    def test_co_authored_by(self):
        email = self.from_patch_glob('1850')
        assert email.co_authors == ['Jakub Jelinek  <jakub@redhat.com>']
        output_entries = list(email.to_changelog_entries())
        assert len(output_entries) == 2
        ent0 = output_entries[0]
        assert ent0[1].startswith('2020-04-16  Martin Liska  '
                                  '<mliska@suse.cz>\n\t'
                                  '    Jakub Jelinek  <jakub@redhat.com>')

    def test_multiple_co_author_formats(self):
        email = self.get_git_email('co-authored-by.patch')
        assert len(email.co_authors) == 3
        assert email.co_authors[0] == 'Jakub Jelinek  <jakub@redhat.com>'
        assert email.co_authors[1] == 'John Miller  <jm@example.com>'
        assert email.co_authors[2] == 'John Miller2  <jm2@example.com>'

    def test_new_file_added_entry(self):
        email = self.from_patch_glob('1957')
        output_entries = list(email.to_changelog_entries())
        assert len(output_entries) == 2
        needle = ('\t* g++.dg/cpp2a/lambda-generic-variadic20.C'
                  ': New file.')
        assert output_entries[1][1].endswith(needle)
        assert email.changelog_entries[1].prs == ['PR c++/94546']

    def test_global_pr_entry(self):
        email = self.from_patch_glob('2004')
        assert not email.errors
        assert email.changelog_entries[0].prs == ['PR other/94629']

    def test_unique_prs(self):
        email = self.get_git_email('pr-check1.patch')
        assert not email.errors
        assert email.changelog_entries[0].prs == ['PR ipa/12345']
        assert email.changelog_entries[1].prs == []

    def test_multiple_prs_not_added(self):
        email = self.from_patch_glob('0002-Add-patch_are')
        assert not email.errors
        assert email.changelog_entries[0].prs == ['PR target/93492']
        assert email.changelog_entries[1].prs == ['PR target/12345']
        assert email.changelog_entries[2].prs == []
        assert email.changelog_entries[2].folder == 'gcc/testsuite'

    def test_strict_mode(self):
        email = self.from_patch_glob('0001-Add-patch_are')
        msg = 'ChangeLog, DATESTAMP, BASE-VER and DEV-PHASE updates should ' \
              'be done separately from normal commits'
        assert email.errors[0].message == msg

    def test_strict_mode_normal_patch(self):
        email = self.get_git_email('0001-Just-test-it.patch')
        assert not email.errors

    def test_strict_mode_datestamp_only(self):
        email = self.get_git_email('0002-Bump-date.patch')
        assert not email.errors

    def test_wrong_changelog_entry(self):
        email = self.from_patch_glob('0020-IPA-Avoid')
        msg = 'first line should start with a tab, an asterisk and a space'
        assert (email.errors[0].message == msg)

    def test_cherry_pick_format(self):
        email = self.from_patch_glob('0001-c-Alias.patch')
        assert not email.errors

    def test_signatures(self):
        email = self.from_patch_glob('0001-RISC-V-Make-unique.patch')
        assert not email.errors
        assert len(email.changelog_entries) == 1

    def test_duplicate_top_level_author(self):
        email = self.from_patch_glob('0001-Fortran-ProcPtr-function.patch')
        assert not email.errors
        assert len(email.changelog_entries[0].author_lines) == 1

    def test_dr_entry(self):
        email = self.from_patch_glob('0001-c-C-20-DR-2237.patch')
        assert email.changelog_entries[0].prs == ['DR 2237']

    def test_changes_only_in_ignored_location(self):
        email = self.from_patch_glob('0001-go-in-ignored-location.patch')
        assert not email.errors

    def test_changelog_for_ignored_location(self):
        email = self.from_patch_glob('0001-Update-merge.sh-to-reflect.patch')
        assert (email.changelog_entries[0].lines[0]
                == '\t* LOCAL_PATCHES: Use git hash instead of SVN id.')

    def test_multiline_file_list(self):
        email = self.from_patch_glob(
            '0001-Ada-Reuse-Is_Package_Or_Generic_Package-where-possib.patch')
        assert (email.changelog_entries[0].files
                == ['contracts.adb', 'einfo.adb', 'exp_ch9.adb',
                    'sem_ch12.adb', 'sem_ch4.adb', 'sem_ch7.adb',
                    'sem_ch8.adb', 'sem_elab.adb', 'sem_type.adb',
                    'sem_util.adb'])

    @unittest.skipIf(not unidiff_supports_renaming,
                     'Newer version of unidiff is needed (0.6.0+)')
    def test_renamed_file(self):
        email = self.from_patch_glob(
            '0001-Ada-Add-support-for-XDR-streaming-in-the-default-run.patch')
        assert not email.errors

    def test_duplicite_author_lines(self):
        email = self.from_patch_glob('0001-Fortran-type-is-real-kind-1.patch')
        assert (email.changelog_entries[0].author_lines[0][0]
                == 'Steven G. Kargl  <kargl@gcc.gnu.org>')
        assert (email.changelog_entries[0].author_lines[1][0]
                == 'Mark Eggleston  <markeggleston@gcc.gnu.org>')

    def test_missing_change_description(self):
        email = self.from_patch_glob('0001-Missing-change-description.patch')
        assert len(email.errors) == 2
        assert email.errors[0].message == 'missing description of a change'
        assert email.errors[1].message == 'missing description of a change'

    def test_libstdcxx_html_regenerated(self):
        email = self.from_patch_glob('0001-Fix-text-of-hyperlink')
        assert not email.errors
        email = self.from_patch_glob('0002-libstdc-Fake-test-change-1.patch')
        assert len(email.errors) == 1
        msg = "pattern doesn't match any changed files"
        assert email.errors[0].message == msg
        assert email.errors[0].line == 'libstdc++-v3/doc/html/'
        email = self.from_patch_glob('0003-libstdc-Fake-test-change-2.patch')
        assert len(email.errors) == 1
        msg = 'changed file not mentioned in a ChangeLog'
        assert email.errors[0].message == msg

    def test_not_deduce(self):
        email = self.from_patch_glob('0001-configure.patch')
        assert not email.errors
        assert len(email.changelog_entries) == 2

    def test_parse_git_name_status(self):
        modified_files = GitCommit.parse_git_name_status(NAME_STATUS1)
        assert len(modified_files) == 3
        assert modified_files[1] == ('gcc/ada/libgnat/s-atopar.adb', 'D')
        assert modified_files[2] == ('gcc/ada/libgnat/s-aoinar.adb', 'A')

    def test_backport(self):
        email = self.from_patch_glob('0001-asan-fix-RTX-emission.patch')
        assert not email.errors
        expected_hash = '8cff672cb9a132d3d3158c2edfc9a64b55292b80'
        assert email.cherry_pick_commit == expected_hash
        assert len(email.changelog_entries) == 1
        entry = list(email.to_changelog_entries())[0][1]
        assert entry.startswith('2020-06-11  Martin Liska  <mliska@suse.cz>')
        assert '\tBackported from master:' in entry
        assert '\t2020-06-11  Martin Liska  <mliska@suse.cz>' in entry
        assert '\t\t    Jakub Jelinek  <jakub@redhat.com>' in entry

    def test_backport_double_cherry_pick(self):
        email = self.from_patch_glob('double-cherry-pick.patch')
        assert email.errors[0].message.startswith('multiple cherry pick lines')

    def test_square_and_lt_gt(self):
        email = self.from_patch_glob('0001-Check-for-more-missing')
        assert not email.errors

    def test_empty_parenthesis(self):
        email = self.from_patch_glob('0001-tree-optimization-97633-fix')
        assert len(email.errors) == 1
        assert email.errors[0].message == 'empty group "()" found'

    def test_emptry_entry_desc(self):
        email = self.from_patch_glob('0001-c-Set-CALL_FROM_NEW_OR')
        assert len(email.errors) == 1
        assert email.errors[0].message == 'missing description of a change'

    def test_emptry_entry_desc_2(self):
        email = self.from_patch_glob('0001-lto-fix-LTO-debug')
        assert not email.errors
        assert len(email.changelog_entries) == 1

    def test_wildcard_in_subdir(self):
        email = self.from_patch_glob('0001-Wildcard-subdirs.patch')
        assert len(email.changelog_entries) == 1
        err = email.errors[0]
        assert err.message == "pattern doesn't match any changed files"
        assert err.line == 'libstdc++-v3/testsuite/28_regex_not-existing/'

    def test_unicode_chars_in_filename(self):
        email = self.from_patch_glob('0001-Add-horse.patch')
        assert not email.errors

    def test_bad_unicode_chars_in_filename(self):
        email = self.from_patch_glob('0001-Add-horse2.patch')
        assert not email.errors
        assert email.changelog_entries[0].files == ['koníček.txt']

    def test_modification_of_old_changelog(self):
        email = self.from_patch_glob('0001-fix-old-ChangeLog.patch')
        assert not email.errors

    def test_multiline_parentheses(self):
        email = self.from_patch_glob('0001-Add-macro.patch')
        assert not email.errors

    def test_multiline_bad_parentheses(self):
        email = self.from_patch_glob('0002-Wrong-macro-changelog.patch')
        assert email.errors[0].message == 'bad parentheses wrapping'

    def test_changelog_removal(self):
        email = self.from_patch_glob('0001-ChangeLog-removal.patch')
        assert not email.errors

    def test_long_filenames(self):
        email = self.from_patch_glob('0001-long-filenames')
        assert not email.errors

    def test_multi_same_file(self):
        email = self.from_patch_glob('0001-OpenMP-Fix-SIMT')
        assert email.errors[0].message == 'same file specified multiple times'
