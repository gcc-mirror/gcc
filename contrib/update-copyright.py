#!/usr/bin/python
#
# Copyright (C) 2013 Free Software Foundation, Inc.
#
# This script is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This script adjusts the copyright notices at the top of source files
# so that they have the form:
#
#   Copyright XXXX-YYYY Free Software Foundation, Inc.
#
# It doesn't change code that is known to be maintained elsewhere or
# that carries a non-FSF copyright.
#
# The script also doesn't change testsuite files, except those in
# libstdc++-v3.  This is because libstdc++-v3 has a conformance testsuite,
# while most tests in other directories are just things that failed at some
# point in the past.
#
# Pass --this-year to the script if you want it to add the current year
# to all applicable notices.  Pass --quilt if you are using quilt and
# want files to be added to the quilt before being changed.
#
# By default the script will update all directories for which the
# output has been vetted.  You can instead pass the names of individual
# directories, including those that haven't been approved.  So:
#
#    update-copyright.pl --this-year
#
# is the command that would be used at the beginning of a year to update
# all copyright notices (and possibly at other times to check whether
# new files have been added with old years).  On the other hand:
#
#    update-copyright.pl --this-year libjava
#
# would run the script on just libjava/.
#
# Note that things like --version output strings must be updated before
# this script is run.  There's already a separate procedure for that.

import os
import re
import sys
import time
import subprocess

class Errors:
    def __init__ (self):
        self.num_errors = 0

    def report (self, filename, string):
        if filename:
            string = filename + ': ' + string
        sys.stderr.write (string + '\n')
        self.num_errors += 1

    def ok (self):
        return self.num_errors == 0

class GenericFilter:
    def __init__ (self):
        self.skip_files = set()
        self.skip_dirs = set()
        self.skip_extensions = set()
        self.fossilised_files = set()
        self.own_files = set()

        self.skip_files |= set ([
                # Skip licence files.
                'COPYING',
                'COPYING.LIB',
                'COPYING3',
                'COPYING3.LIB',
                'LICENSE',
                'fdl.texi',
                'gpl_v3.texi',
                'fdl-1.3.xml',
                'gpl-3.0.xml',

                # Skip auto- and libtool-related files
                'aclocal.m4',
                'compile',
                'config.guess',
                'config.sub',
                'depcomp',
                'install-sh',
                'libtool.m4',
                'ltmain.sh',
                'ltoptions.m4',
                'ltsugar.m4',
                'ltversion.m4',
                'lt~obsolete.m4',
                'missing',
                'mkdep',
                'mkinstalldirs',
                'move-if-change',
                'shlibpath.m4',
                'symlink-tree',
                'ylwrap',

                # Skip FSF mission statement, etc.
                'gnu.texi',
                'funding.texi',
                'appendix_free.xml',

                # Skip imported texinfo files.
                'texinfo.tex',
                ])


    def get_line_filter (self, dir, filename):
        if filename.startswith ('ChangeLog'):
            # Ignore references to copyright in changelog entries.
            return re.compile ('\t')

        return None

    def skip_file (self, dir, filename):
        if filename in self.skip_files:
            return True

        (base, extension) = os.path.splitext (os.path.join (dir, filename))
        if extension in self.skip_extensions:
            return True

        if extension == '.in':
            # Skip .in files produced by automake.
            if os.path.exists (base + '.am'):
                return True

            # Skip files produced by autogen
            if (os.path.exists (base + '.def')
                and os.path.exists (base + '.tpl')):
                return True

        # Skip configure files produced by autoconf
        if filename == 'configure':
            if os.path.exists (base + '.ac'):
                return True
            if os.path.exists (base + '.in'):
                return True

        return False

    def skip_dir (self, dir, subdir):
        return subdir in self.skip_dirs

    def is_fossilised_file (self, dir, filename):
        if filename in self.fossilised_files:
            return True
        # Only touch current current ChangeLogs.
        if filename != 'ChangeLog' and filename.find ('ChangeLog') >= 0:
            return True
        return False

    def by_package_author (self, dir, filename):
        return filename in self.own_files

class Copyright:
    def __init__ (self, errors):
        self.errors = errors

        # Characters in a range of years.  Include '.' for typos.
        ranges = '[0-9](?:[-0-9.,\s]|\s+and\s+)*[0-9]'

        # Non-whitespace characters in a copyright holder's name.
        name = '[\w.,-]'

        # Matches one year.
        self.year_re = re.compile ('[0-9]+')

        # Matches part of a year or copyright holder.
        self.continuation_re = re.compile (ranges + '|' + name)

        # Matches a full copyright notice:
        self.copyright_re = re.compile (
            # 1: 'Copyright (C)', etc.
            '([Cc]opyright'
            '|[Cc]opyright\s+\([Cc]\)'
            '|[Cc]opyright\s+%s'
            '|[Cc]opyright\s+&copy;'
            '|[Cc]opyright\s+@copyright{}'
            '|@set\s+copyright[\w-]+)'

            # 2: the years.  Include the whitespace in the year, so that
            # we can remove any excess.
            '(\s*(?:' + ranges + ',?'
            '|@value\{[^{}]*\})\s*)'

            # 3: 'by ', if used
            '(by\s+)?'

            # 4: the copyright holder.  Don't allow multiple consecutive
            # spaces, so that right-margin gloss doesn't get caught
            # (e.g. gnat_ugn.texi).
            '(' + name + '(?:\s?' + name + ')*)?')

        # A regexp for notices that might have slipped by.  Just matching
        # 'copyright' is too noisy, and 'copyright.*[0-9]' falls foul of
        # HTML header markers, so check for 'copyright' and two digits.
        self.other_copyright_re = re.compile ('copyright.*[0-9][0-9]',
                                              re.IGNORECASE)
        self.comment_re = re.compile('#+|[*]+|;+|%+|//+|@c |dnl ')
        self.holders = { '@copying': '@copying' }
        self.holder_prefixes = set()

        # True to 'quilt add' files before changing them.
        self.use_quilt = False

        # If set, force all notices to include this year.
        self.max_year = None

        # Goes after the year(s).  Could be ', '.
        self.separator = ' '

    def add_package_author (self, holder, canon_form = None):
        if not canon_form:
            canon_form = holder
        self.holders[holder] = canon_form
        index = holder.find (' ')
        while index >= 0:
            self.holder_prefixes.add (holder[:index])
            index = holder.find (' ', index + 1)

    def add_external_author (self, holder):
        self.holders[holder] = None

    class BadYear():
        def __init__ (self, year):
            self.year = year

        def __str__ (self):
            return 'unrecognised year: ' + self.year

    def parse_year (self, string):
        year = int (string)
        if len (string) == 2:
            if year > 70:
                return year + 1900
        elif len (string) == 4:
            return year
        raise self.BadYear (string)

    def year_range (self, years):
        year_list = [self.parse_year (year)
                     for year in self.year_re.findall (years)]
        assert len (year_list) > 0
        return (min (year_list), max (year_list))

    def set_use_quilt (self, use_quilt):
        self.use_quilt = use_quilt

    def include_year (self, year):
        assert not self.max_year
        self.max_year = year

    def canonicalise_years (self, dir, filename, filter, years):
        # Leave texinfo variables alone.
        if years.startswith ('@value'):
            return years

        (min_year, max_year) = self.year_range (years)

        # Update the upper bound, if enabled.
        if self.max_year and not filter.is_fossilised_file (dir, filename):
            max_year = max (max_year, self.max_year)

        # Use a range.
        if min_year == max_year:
            return '%d' % min_year
        else:
            return '%d-%d' % (min_year, max_year)

    def strip_continuation (self, line):
        line = line.lstrip()
        match = self.comment_re.match (line)
        if match:
            line = line[match.end():].lstrip()
        return line

    def is_complete (self, match):
        holder = match.group (4)
        return (holder
                and (holder not in self.holder_prefixes
                     or holder in self.holders))

    def update_copyright (self, dir, filename, filter, file, line, match):
        orig_line = line
        next_line = None
        pathname = os.path.join (dir, filename)

        intro = match.group (1)
        if intro.startswith ('@set'):
            # Texinfo year variables should always be on one line
            after_years = line[match.end (2):].strip()
            if after_years != '':
                self.errors.report (pathname,
                                    'trailing characters in @set: '
                                    + after_years)
                return (False, orig_line, next_line)
        else:
            # If it looks like the copyright is incomplete, add the next line.
            while not self.is_complete (match):
                try:
                    next_line = file.next()
                except StopIteration:
                    break

                # If the next line doesn't look like a proper continuation,
                # assume that what we've got is complete.
                continuation = self.strip_continuation (next_line)
                if not self.continuation_re.match (continuation):
                    break

                # Merge the lines for matching purposes.
                orig_line += next_line
                line = line.rstrip() + ' ' + continuation
                next_line = None

                # Rematch with the longer line, at the original position.
                match = self.copyright_re.match (line, match.start())
                assert match

            holder = match.group (4)

            # Use the filter to test cases where markup is getting in the way.
            if filter.by_package_author (dir, filename):
                assert holder not in self.holders

            elif not holder:
                self.errors.report (pathname, 'missing copyright holder')
                return (False, orig_line, next_line)

            elif holder not in self.holders:
                self.errors.report (pathname,
                                    'unrecognised copyright holder: ' + holder)
                return (False, orig_line, next_line)

            else:
                # See whether the copyright is associated with the package
                # author.
                canon_form = self.holders[holder]
                if not canon_form:
                    return (False, orig_line, next_line)

                # Make sure the author is given in a consistent way.
                line = (line[:match.start (4)]
                        + canon_form
                        + line[match.end (4):])

                # Remove any 'by'
                line = line[:match.start (3)] + line[match.end (3):]

        # Update the copyright years.
        years = match.group (2).strip()
        try:
            canon_form = self.canonicalise_years (dir, filename, filter, years)
        except self.BadYear as e:
            self.errors.report (pathname, str (e))
            return (False, orig_line, next_line)

        line = (line[:match.start (2)]
                + ' ' + canon_form + self.separator
                + line[match.end (2):])

        # Use the standard (C) form.
        if intro.endswith ('right'):
            intro += ' (C)'
        elif intro.endswith ('(c)'):
            intro = intro[:-3] + '(C)'
        line = line[:match.start (1)] + intro + line[match.end (1):]

        # Strip trailing whitespace
        line = line.rstrip() + '\n'

        return (line != orig_line, line, next_line)

    def process_file (self, dir, filename, filter):
        pathname = os.path.join (dir, filename)
        if filename.endswith ('.tmp'):
            # Looks like something we tried to create before.
            try:
                os.remove (pathname)
            except OSError:
                pass
            return

        lines = []
        changed = False
        line_filter = filter.get_line_filter (dir, filename)
        with open (pathname, 'r') as file:
            prev = None
            for line in file:
                while line:
                    next_line = None
                    # Leave filtered-out lines alone.
                    if not (line_filter and line_filter.match (line)):
                        match = self.copyright_re.search (line)
                        if match:
                            res = self.update_copyright (dir, filename, filter,
                                                         file, line, match)
                            (this_changed, line, next_line) = res
                            changed = changed or this_changed

                        # Check for copyright lines that might have slipped by.
                        elif self.other_copyright_re.search (line):
                            self.errors.report (pathname,
                                                'unrecognised copyright: %s'
                                                % line.strip())
                    lines.append (line)
                    line = next_line

        # If something changed, write the new file out.
        if changed and self.errors.ok():
            tmp_pathname = pathname + '.tmp'
            with open (tmp_pathname, 'w') as file:
                for line in lines:
                    file.write (line)
            if self.use_quilt:
                subprocess.call (['quilt', 'add', pathname])
            os.rename (tmp_pathname, pathname)

    def process_tree (self, tree, filter):
        for (dir, subdirs, filenames) in os.walk (tree):
            # Don't recurse through directories that should be skipped.
            for i in xrange (len (subdirs) - 1, -1, -1):
                if filter.skip_dir (dir, subdirs[i]):
                    del subdirs[i]

            # Handle the files in this directory.
            for filename in filenames:
                if filter.skip_file (dir, filename):
                    sys.stdout.write ('Skipping %s\n'
                                      % os.path.join (dir, filename))
                else:
                    self.process_file (dir, filename, filter)

class CmdLine:
    def __init__ (self, copyright = Copyright):
        self.errors = Errors()
        self.copyright = copyright (self.errors)
        self.dirs = []
        self.default_dirs = []
        self.chosen_dirs = []
        self.option_handlers = dict()
        self.option_help = []

        self.add_option ('--help', 'Print this help', self.o_help)
        self.add_option ('--quilt', '"quilt add" files before changing them',
                         self.o_quilt)
        self.add_option ('--this-year', 'Add the current year to every notice',
                         self.o_this_year)

    def add_option (self, name, help, handler):
        self.option_help.append ((name, help))
        self.option_handlers[name] = handler

    def add_dir (self, dir, filter = GenericFilter()):
        self.dirs.append ((dir, filter))

    def o_help (self, option = None):
        sys.stdout.write ('Usage: %s [options] dir1 dir2...\n\n'
                          'Options:\n' % sys.argv[0])
        format = '%-15s %s\n'
        for (what, help) in self.option_help:
            sys.stdout.write (format % (what, help))
        sys.stdout.write ('\nDirectories:\n')

        format = '%-25s'
        i = 0
        for (dir, filter) in self.dirs:
            i += 1
            if i % 3 == 0 or i == len (self.dirs):
                sys.stdout.write (dir + '\n')
            else:
                sys.stdout.write (format % dir)
        sys.exit (0)

    def o_quilt (self, option):
        self.copyright.set_use_quilt (True)

    def o_this_year (self, option):
        self.copyright.include_year (time.localtime().tm_year)

    def main (self):
        for arg in sys.argv[1:]:
            if arg[:1] != '-':
                self.chosen_dirs.append (arg)
            elif arg in self.option_handlers:
                self.option_handlers[arg] (arg)
            else:
                self.errors.report (None, 'unrecognised option: ' + arg)
        if self.errors.ok():
            if len (self.chosen_dirs) == 0:
                self.chosen_dirs = self.default_dirs
            if len (self.chosen_dirs) == 0:
                self.o_help()
            else:
                for chosen_dir in self.chosen_dirs:
                    canon_dir = os.path.join (chosen_dir, '')
                    count = 0
                    for (dir, filter) in self.dirs:
                        if (dir + os.sep).startswith (canon_dir):
                            count += 1
                            self.copyright.process_tree (dir, filter)
                    if count == 0:
                        self.errors.report (None, 'unrecognised directory: '
                                            + chosen_dir)
        sys.exit (0 if self.errors.ok() else 1)

#----------------------------------------------------------------------------

class TopLevelFilter (GenericFilter):
    def skip_dir (self, dir, subdir):
        return True

class ConfigFilter (GenericFilter):
    def __init__ (self):
        GenericFilter.__init__ (self)

    def skip_file (self, dir, filename):
        if filename.endswith ('.m4'):
            pathname = os.path.join (dir, filename)
            with open (pathname) as file:
                # Skip files imported from gettext.
                if file.readline().find ('gettext-') >= 0:
                    return True
        return GenericFilter.skip_file (self, dir, filename)

class GCCFilter (GenericFilter):
    def __init__ (self):
        GenericFilter.__init__ (self)

        self.skip_files |= set ([
                # Not part of GCC
                'math-68881.h',
                ])

        self.skip_dirs |= set ([
                # Better not create a merge nightmare for the GNAT folks.
                'ada',

                # Handled separately.
                'testsuite',
                ])

        self.skip_extensions |= set ([
                # Maintained by the translation project.
                '.po',

                # Automatically-generated.
                '.pot',
                ])

        self.fossilised_files |= set ([
                # Old news won't be updated.
                'ONEWS',
                ])

class TestsuiteFilter (GenericFilter):
    def __init__ (self):
        GenericFilter.__init__ (self)

        self.skip_extensions |= set ([
                # Don't change the tests, which could be woend by anyone.
                '.c',
                '.C',
                '.cc',
                '.h',
                '.hs',
                '.f',
                '.f90',
                '.go',
                '.inc',
                '.java',
                ])

    def skip_file (self, dir, filename):
        # g++.niklas/README contains historical copyright information
        # and isn't updated.
        if filename == 'README' and os.path.basename (dir) == 'g++.niklas':
            return True
        return GenericFilter.skip_file (self, dir, filename)

class LibCppFilter (GenericFilter):
    def __init__ (self):
        GenericFilter.__init__ (self)

        self.skip_extensions |= set ([
                # Maintained by the translation project.
                '.po',

                # Automatically-generated.
                '.pot',
                ])

class LibGCCFilter (GenericFilter):
    def __init__ (self):
        GenericFilter.__init__ (self)

        self.skip_dirs |= set ([
                # Imported from GLIBC.
                'soft-fp',
                ])

class LibJavaFilter (GenericFilter):
    def __init__ (self):
        GenericFilter.__init__ (self)

        self.skip_dirs |= set ([
                # Handled separately.
                'testsuite',

                # Not really part of the library
                'contrib',

                # Imported from upstream
                'classpath',
                'libltdl',
                ])

    def get_line_filter (self, dir, filename):
        if filename == 'NameDecoder.h':
            return re.compile ('.*NAME_COPYRIGHT')
        if filename == 'ICC_Profile.h':
            return re.compile ('.*icSigCopyrightTag')
        return GenericFilter.get_line_filter (self, dir, filename)

class LibMudflapFilter (GenericFilter):
    def __init__ (self):
        GenericFilter.__init__ (self)

        self.skip_dirs |= set ([
                # Handled separately.
                'testsuite',
                ])

class LibStdCxxFilter (GenericFilter):
    def __init__ (self):
        GenericFilter.__init__ (self)

        self.skip_files |= set ([
                # Contains no copyright of its own, but quotes the GPL.
                'intro.xml',
                ])

        self.skip_dirs |= set ([
                # Contains automatically-generated sources.
                'html',

                # The testsuite data files shouldn't be changed.
                'data',

                # Contains imported images
                'images',
                ])

        self.own_files |= set ([
                # Contains markup around the copyright owner.
                'spine.xml',
                ])

    def get_line_filter (self, dir, filename):
        if filename == 'boost_concept_check.h':
            return re.compile ('// \(C\) Copyright Jeremy Siek')
        return GenericFilter.get_line_filter (self, dir, filename)

class GCCCopyright (Copyright):
    def __init__ (self, errors):
        Copyright.__init__ (self, errors)

        canon_fsf = 'Free Software Foundation, Inc.'
        self.add_package_author ('Free Software Foundation', canon_fsf)
        self.add_package_author ('Free Software Foundation.', canon_fsf)
        self.add_package_author ('Free Software Foundation Inc.', canon_fsf)
        self.add_package_author ('Free Software Foundation, Inc', canon_fsf)
        self.add_package_author ('Free Software Foundation, Inc.', canon_fsf)
        self.add_package_author ('The Free Software Foundation', canon_fsf)
        self.add_package_author ('The Free Software Foundation, Inc.', canon_fsf)
        self.add_package_author ('Software Foundation, Inc.', canon_fsf)

        self.add_external_author ('ARM')
        self.add_external_author ('AdaCore')
        self.add_external_author ('Ami Tavory and Vladimir Dreizin, IBM-HRL.')
        self.add_external_author ('Cavium Networks.')
        self.add_external_author ('Faraday Technology Corp.')
        self.add_external_author ('Florida State University')
        self.add_external_author ('Greg Colvin and Beman Dawes.')
        self.add_external_author ('Hewlett-Packard Company')
        self.add_external_author ('Information Technology Industry Council.')
        self.add_external_author ('James Theiler, Brian Gough')
        self.add_external_author ('Makoto Matsumoto and Takuji Nishimura,')
        self.add_external_author ('National Research Council of Canada.')
        self.add_external_author ('Peter Dimov and Multi Media Ltd.')
        self.add_external_author ('Peter Dimov')
        self.add_external_author ('Pipeline Associates, Inc.')
        self.add_external_author ('Regents of the University of California.')
        self.add_external_author ('Silicon Graphics Computer Systems, Inc.')
        self.add_external_author ('Silicon Graphics')
        self.add_external_author ('Stephen L. Moshier')
        self.add_external_author ('Sun Microsystems, Inc. All rights reserved.')
        self.add_external_author ('The Go Authors.  All rights reserved.')
        self.add_external_author ('The Go Authors. All rights reserved.')
        self.add_external_author ('The Go Authors.')
        self.add_external_author ('The Regents of the University of California.')
        self.add_external_author ('Unicode, Inc.')
        self.add_external_author ('University of Toronto.')

class GCCCmdLine (CmdLine):
    def __init__ (self):
        CmdLine.__init__ (self, GCCCopyright)

        self.add_dir ('.', TopLevelFilter())
        # boehm-gc is imported from upstream.
        self.add_dir ('config', ConfigFilter())
        # contrib isn't really part of GCC.
        self.add_dir ('fixincludes')
        self.add_dir ('gcc', GCCFilter())
        self.add_dir (os.path.join ('gcc', 'testsuite'), TestsuiteFilter())
        self.add_dir ('gnattools')
        self.add_dir ('include')
        self.add_dir ('libada')
        self.add_dir ('libatomic')
        self.add_dir ('libbacktrace')
        self.add_dir ('libcpp', LibCppFilter())
        self.add_dir ('libdecnumber')
        # libffi is imported from upstream.
        self.add_dir ('libgcc', LibGCCFilter())
        self.add_dir ('libgfortran')
        self.add_dir ('libgomp')
        self.add_dir ('libiberty')
        self.add_dir ('libitm')
        self.add_dir ('libjava', LibJavaFilter())
        self.add_dir (os.path.join ('libjava', 'testsuite'), TestsuiteFilter())
        self.add_dir ('libmudflap', LibMudflapFilter())
        self.add_dir (os.path.join ('libmudflap', 'testsuite'),
                      TestsuiteFilter())
        self.add_dir ('libobjc')
        self.add_dir ('libquadmath')
        # libsanitiser is imported from upstream.
        self.add_dir ('libssp')
        self.add_dir ('libstdc++-v3', LibStdCxxFilter())
        self.add_dir ('lto-plugin')
        # zlib is imported from upstream.

        self.default_dirs = [
            'gcc',
            'libada',
            'libatomic',
            'libbacktrace',
            'libcpp',
            'libdecnumber',
            'libgcc',
            'libgfortran',
            'libgomp',
            'libitm',
            'libmudflap',
            'libobjc',
            'libstdc++-v3',
            ]

GCCCmdLine().main()
