#!/usr/bin/python
#
# Copyright (C) 2014 Free Software Foundation, Inc.
#
# This script is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

import sys
import getopt
import re
import io
from datetime import datetime
from operator import attrgetter

# True if unrecognised lines should cause a fatal error.  Might want to turn
# this on by default later.
strict = False

# True if the order of .log segments should match the .sum file, false if
# they should keep the original order.
sort_logs = True

# A version of open() that is safe against whatever binary output
# might be added to the log.
def safe_open (filename):
    if sys.version_info >= (3, 0):
        return open (filename, 'r', errors = 'surrogateescape')
    return open (filename, 'r')

# Force stdout to handle escape sequences from a safe_open file.
if sys.version_info >= (3, 0):
    sys.stdout = io.TextIOWrapper (sys.stdout.buffer,
                                   errors = 'surrogateescape')

class Named:
    def __init__ (self, name):
        self.name = name

class ToolRun (Named):
    def __init__ (self, name):
        Named.__init__ (self, name)
        # The variations run for this tool, mapped by --target_board name.
        self.variations = dict()

    # Return the VariationRun for variation NAME.
    def get_variation (self, name):
        if name not in self.variations:
            self.variations[name] = VariationRun (name)
        return self.variations[name]

class VariationRun (Named):
    def __init__ (self, name):
        Named.__init__ (self, name)
        # A segment of text before the harness runs start, describing which
        # baseboard files were loaded for the target.
        self.header = None
        # The harnesses run for this variation, mapped by filename.
        self.harnesses = dict()
        # A list giving the number of times each type of result has
        # been seen.
        self.counts = []

    # Return the HarnessRun for harness NAME.
    def get_harness (self, name):
        if name not in self.harnesses:
            self.harnesses[name] = HarnessRun (name)
        return self.harnesses[name]

class HarnessRun (Named):
    def __init__ (self, name):
        Named.__init__ (self, name)
        # Segments of text that make up the harness run, mapped by a test-based
        # key that can be used to order them.
        self.segments = dict()
        # Segments of text that make up the harness run but which have
        # no recognized test results.  These are typically harnesses that
        # are completely skipped for the target.
        self.empty = []
        # A list of results.  Each entry is a pair in which the first element
        # is a unique sorting key and in which the second is the full
        # PASS/FAIL line.
        self.results = []

    # Add a segment of text to the harness run.  If the segment includes
    # test results, KEY is an example of one of them, and can be used to
    # combine the individual segments in order.  If the segment has no
    # test results (e.g. because the harness doesn't do anything for the
    # current configuration) then KEY is None instead.  In that case
    # just collect the segments in the order that we see them.
    def add_segment (self, key, segment):
        if key:
            assert key not in self.segments
            self.segments[key] = segment
        else:
            self.empty.append (segment)

class Segment:
    def __init__ (self, filename, start):
        self.filename = filename
        self.start = start
        self.lines = 0

class Prog:
    def __init__ (self):
        # The variations specified on the command line.
        self.variations = []
        # The variations seen in the input files.
        self.known_variations = set()
        # The tools specified on the command line.
        self.tools = []
        # Whether to create .sum rather than .log output.
        self.do_sum = True
        # Regexps used while parsing.
        self.test_run_re = re.compile (r'^Test Run By (\S+) on (.*)$')
        self.tool_re = re.compile (r'^\t\t=== (.*) tests ===$')
        self.result_re = re.compile (r'^(PASS|XPASS|FAIL|XFAIL|UNRESOLVED'
                                     r'|WARNING|ERROR|UNSUPPORTED|UNTESTED'
                                     r'|KFAIL):\s*(\S+)')
        self.completed_re = re.compile (r'.* completed at (.*)')
        # Pieces of text to write at the head of the output.
        # start_line is a pair in which the first element is a datetime
        # and in which the second is the associated 'Test Run By' line.
        self.start_line = None
        self.native_line = ''
        self.target_line = ''
        self.host_line = ''
        self.acats_premable = ''
        # Pieces of text to write at the end of the output.
        # end_line is like start_line but for the 'runtest completed' line.
        self.acats_failures = []
        self.version_output = ''
        self.end_line = None
        # Known summary types.
        self.count_names = [
            '# of expected passes\t\t',
            '# of unexpected failures\t',
            '# of unexpected successes\t',
            '# of expected failures\t\t',
            '# of unknown successes\t\t',
            '# of known failures\t\t',
            '# of untested testcases\t\t',
            '# of unresolved testcases\t',
            '# of unsupported tests\t\t'
        ]
        self.runs = dict()

    def usage (self):
        name = sys.argv[0]
        sys.stderr.write ('Usage: ' + name
                          + ''' [-t tool] [-l variant-list] [-L] log-or-sum-file ...

    tool           The tool (e.g. g++, libffi) for which to create a
                   new test summary file.  If not specified then output
                   is created for all tools.
    variant-list   One or more test variant names.  If the list is
                   not specified then one is constructed from all
                   variants in the files for <tool>.
    sum-file       A test summary file with the format of those
                   created by runtest from DejaGnu.
    If -L is used, merge *.log files instead of *.sum.  In this
    mode the exact order of lines may not be preserved, just different
    Running *.exp chunks should be in correct order.
''')
        sys.exit (1)

    def fatal (self, what, string):
        if not what:
            what = sys.argv[0]
        sys.stderr.write (what + ': ' + string + '\n')
        sys.exit (1)

    # Parse the command-line arguments.
    def parse_cmdline (self):
        try:
            (options, self.files) = getopt.getopt (sys.argv[1:], 'l:t:L')
            if len (self.files) == 0:
                self.usage()
            for (option, value) in options:
                if option == '-l':
                    self.variations.append (value)
                elif option == '-t':
                    self.tools.append (value)
                else:
                    self.do_sum = False
        except getopt.GetoptError as e:
            self.fatal (None, e.msg)

    # Try to parse time string TIME, returning an arbitrary time on failure.
    # Getting this right is just a nice-to-have so failures should be silent.
    def parse_time (self, time):
        try:
            return datetime.strptime (time, '%c')
        except ValueError:
            return datetime.now()

    # Parse an integer and abort on failure.
    def parse_int (self, filename, value):
        try:
            return int (value)
        except ValueError:
            self.fatal (filename, 'expected an integer, got: ' + value)

    # Return a list that represents no test results.
    def zero_counts (self):
        return [0 for x in self.count_names]

    # Return the ToolRun for tool NAME.
    def get_tool (self, name):
        if name not in self.runs:
            self.runs[name] = ToolRun (name)
        return self.runs[name]

    # Add the result counts in list FROMC to TOC.
    def accumulate_counts (self, toc, fromc):
        for i in range (len (self.count_names)):
            toc[i] += fromc[i]

    # Parse the list of variations after 'Schedule of variations:'.
    # Return the number seen.
    def parse_variations (self, filename, file):
        num_variations = 0
        while True:
            line = file.readline()
            if line == '':
                self.fatal (filename, 'could not parse variation list')
            if line == '\n':
                break
            self.known_variations.add (line.strip())
            num_variations += 1
        return num_variations

    # Parse from the first line after 'Running target ...' to the end
    # of the run's summary.
    def parse_run (self, filename, file, tool, variation, num_variations):
        header = None
        harness = None
        segment = None
        final_using = 0

        # If this is the first run for this variation, add any text before
        # the first harness to the header.
        if not variation.header:
            segment = Segment (filename, file.tell())
            variation.header = segment

        # Parse up until the first line of the summary.
        if num_variations == 1:
            end = '\t\t=== ' + tool.name + ' Summary ===\n'
        else:
            end = ('\t\t=== ' + tool.name + ' Summary for '
                   + variation.name + ' ===\n')
        while True:
            line = file.readline()
            if line == '':
                self.fatal (filename, 'no recognised summary line')
            if line == end:
                break

            # Look for the start of a new harness.
            if line.startswith ('Running ') and line.endswith (' ...\n'):
                # Close off the current harness segment, if any.
                if harness:
                    segment.lines -= final_using
                    harness.add_segment (first_key, segment)
                name = line[len ('Running '):-len(' ...\n')]
                harness = variation.get_harness (name)
                segment = Segment (filename, file.tell())
                first_key = None
                final_using = 0
                continue

            # Record test results.  Associate the first test result with
            # the harness segment, so that if a run for a particular harness
            # has been split up, we can reassemble the individual segments
            # in a sensible order.
            #
            # dejagnu sometimes issues warnings about the testing environment
            # before running any tests.  Treat them as part of the header
            # rather than as a test result.
            match = self.result_re.match (line)
            if match and (harness or not line.startswith ('WARNING:')):
                if not harness:
                    self.fatal (filename, 'saw test result before harness name')
                name = match.group (2)
                # Ugly hack to get the right order for gfortran.
                if name.startswith ('gfortran.dg/g77/'):
                    name = 'h' + name
                key = (name, len (harness.results))
                harness.results.append ((key, line))
                if not first_key and sort_logs:
                    first_key = key

            # 'Using ...' lines are only interesting in a header.  Splitting
            # the test up into parallel runs leads to more 'Using ...' lines
            # than there would be in a single log.
            if line.startswith ('Using '):
                final_using += 1
            else:
                final_using = 0

            # Add other text to the current segment, if any.
            if segment:
                segment.lines += 1

        # Close off the final harness segment, if any.
        if harness:
            segment.lines -= final_using
            harness.add_segment (first_key, segment)

        # Parse the rest of the summary (the '# of ' lines).
        if len (variation.counts) == 0:
            variation.counts = self.zero_counts()
        while True:
            before = file.tell()
            line = file.readline()
            if line == '':
                break
            if line == '\n':
                continue
            if not line.startswith ('# '):
                file.seek (before)
                break
            found = False
            for i in range (len (self.count_names)):
                if line.startswith (self.count_names[i]):
                    count = line[len (self.count_names[i]):-1].strip()
                    variation.counts[i] += self.parse_int (filename, count)
                    found = True
                    break
            if not found:
                self.fatal (filename, 'unknown test result: ' + line[:-1])

    # Parse an acats run, which uses a different format from dejagnu.
    # We have just skipped over '=== acats configuration ==='.
    def parse_acats_run (self, filename, file):
        # Parse the preamble, which describes the configuration and logs
        # the creation of support files.
        record = (self.acats_premable == '')
        if record:
            self.acats_premable = '\t\t=== acats configuration ===\n'
        while True:
            line = file.readline()
            if line == '':
                self.fatal (filename, 'could not parse acats preamble')
            if line == '\t\t=== acats tests ===\n':
                break
            if record:
                self.acats_premable += line

        # Parse the test results themselves, using a dummy variation name.
        tool = self.get_tool ('acats')
        variation = tool.get_variation ('none')
        self.parse_run (filename, file, tool, variation, 1)

        # Parse the failure list.
        while True:
            before = file.tell()
            line = file.readline()
            if line.startswith ('*** FAILURES: '):
                self.acats_failures.append (line[len ('*** FAILURES: '):-1])
                continue
            file.seek (before)
            break

    # Parse the final summary at the end of a log in order to capture
    # the version output that follows it.
    def parse_final_summary (self, filename, file):
        record = (self.version_output == '')
        while True:
            line = file.readline()
            if line == '':
                break
            if line.startswith ('# of '):
                continue
            if record:
                self.version_output += line
            if line == '\n':
                break

    # Parse a .log or .sum file.
    def parse_file (self, filename, file):
        tool = None
        target = None
        num_variations = 1
        while True:
            line = file.readline()
            if line == '':
                return

            # Parse the list of variations, which comes before the test
            # runs themselves.
            if line.startswith ('Schedule of variations:'):
                num_variations = self.parse_variations (filename, file)
                continue

            # Parse a testsuite run for one tool/variation combination.
            if line.startswith ('Running target '):
                name = line[len ('Running target '):-1]
                if not tool:
                    self.fatal (filename, 'could not parse tool name')
                if name not in self.known_variations:
                    self.fatal (filename, 'unknown target: ' + name)
                self.parse_run (filename, file, tool,
                                tool.get_variation (name),
                                num_variations)
                # If there is only one variation then there is no separate
                # summary for it.  Record any following version output.
                if num_variations == 1:
                    self.parse_final_summary (filename, file)
                continue

            # Parse the start line.  In the case where several files are being
            # parsed, pick the one with the earliest time.
            match = self.test_run_re.match (line)
            if match:
                time = self.parse_time (match.group (2))
                if not self.start_line or self.start_line[0] > time:
                    self.start_line = (time, line)
                continue

            # Parse the form used for native testing.
            if line.startswith ('Native configuration is '):
                self.native_line = line
                continue

            # Parse the target triplet.
            if line.startswith ('Target is '):
                self.target_line = line
                continue

            # Parse the host triplet.
            if line.startswith ('Host   is '):
                self.host_line = line
                continue

            # Parse the acats premable.
            if line == '\t\t=== acats configuration ===\n':
                self.parse_acats_run (filename, file)
                continue

            # Parse the tool name.
            match = self.tool_re.match (line)
            if match:
                tool = self.get_tool (match.group (1))
                continue

            # Skip over the final summary (which we instead create from
            # individual runs) and parse the version output.
            if tool and line == '\t\t=== ' + tool.name + ' Summary ===\n':
                if file.readline() != '\n':
                    self.fatal (filename, 'expected blank line after summary')
                self.parse_final_summary (filename, file)
                continue

            # Parse the completion line.  In the case where several files
            # are being parsed, pick the one with the latest time.
            match = self.completed_re.match (line)
            if match:
                time = self.parse_time (match.group (1))
                if not self.end_line or self.end_line[0] < time:
                    self.end_line = (time, line)
                continue

            # Sanity check to make sure that important text doesn't get
            # dropped accidentally.
            if strict and line.strip() != '':
                self.fatal (filename, 'unrecognised line: ' + line[:-1])

    # Output a segment of text.
    def output_segment (self, segment):
        with safe_open (segment.filename) as file:
            file.seek (segment.start)
            for i in range (segment.lines):
                sys.stdout.write (file.readline())

    # Output a summary giving the number of times each type of result has
    # been seen.
    def output_summary (self, tool, counts):
        for i in range (len (self.count_names)):
            name = self.count_names[i]
            # dejagnu only prints result types that were seen at least once,
            # but acats always prints a number of unexpected failures.
            if (counts[i] > 0
                or (tool.name == 'acats'
                    and name.startswith ('# of unexpected failures'))):
                sys.stdout.write ('%s%d\n' % (name, counts[i]))

    # Output unified .log or .sum information for a particular variation,
    # with a summary at the end.
    def output_variation (self, tool, variation):
        self.output_segment (variation.header)
        for harness in sorted (variation.harnesses.values(),
                               key = attrgetter ('name')):
            sys.stdout.write ('Running ' + harness.name + ' ...\n')
            if self.do_sum:
                # Keep the original test result order if there was only
                # one segment for this harness.  This is needed for
                # unsorted.exp, which has unusual test names.  Otherwise
                # sort the tests by test filename.  If there are several
                # subtests for the same test filename (such as 'compilation',
                # 'test for excess errors', etc.) then keep the subtests
                # in the original order.
                if len (harness.segments) > 1:
                    harness.results.sort()
                for (key, line) in harness.results:
                    sys.stdout.write (line)
            else:
                # Rearrange the log segments into test order (but without
                # rearranging text within those segments).
                for key in sorted (harness.segments.keys()):
                    self.output_segment (harness.segments[key])
                for segment in harness.empty:
                    self.output_segment (segment)
        if len (self.variations) > 1:
            sys.stdout.write ('\t\t=== ' + tool.name + ' Summary for '
                              + variation.name + ' ===\n\n')
            self.output_summary (tool, variation.counts)

    # Output unified .log or .sum information for a particular tool,
    # with a summary at the end.
    def output_tool (self, tool):
        counts = self.zero_counts()
        if tool.name == 'acats':
            # acats doesn't use variations, so just output everything.
            # It also has a different approach to whitespace.
            sys.stdout.write ('\t\t=== ' + tool.name + ' tests ===\n')
            for variation in tool.variations.values():
                self.output_variation (tool, variation)
                self.accumulate_counts (counts, variation.counts)
            sys.stdout.write ('\t\t=== ' + tool.name + ' Summary ===\n')
        else:
            # Output the results in the usual dejagnu runtest format.
            sys.stdout.write ('\n\t\t=== ' + tool.name + ' tests ===\n\n'
                              'Schedule of variations:\n')
            for name in self.variations:
                if name in tool.variations:
                    sys.stdout.write ('    ' + name + '\n')
            sys.stdout.write ('\n')
            for name in self.variations:
                if name in tool.variations:
                    variation = tool.variations[name]
                    sys.stdout.write ('Running target '
                                      + variation.name + '\n')
                    self.output_variation (tool, variation)
                    self.accumulate_counts (counts, variation.counts)
            sys.stdout.write ('\n\t\t=== ' + tool.name + ' Summary ===\n\n')
        self.output_summary (tool, counts)

    def main (self):
        self.parse_cmdline()
        try:
            # Parse the input files.
            for filename in self.files:
                with safe_open (filename) as file:
                    self.parse_file (filename, file)

            # Decide what to output.
            if len (self.variations) == 0:
                self.variations = sorted (self.known_variations)
            else:
                for name in self.variations:
                    if name not in self.known_variations:
                        self.fatal (None, 'no results for ' + name)
            if len (self.tools) == 0:
                self.tools = sorted (self.runs.keys())

            # Output the header.
            if self.start_line:
                sys.stdout.write (self.start_line[1])
            sys.stdout.write (self.native_line)
            sys.stdout.write (self.target_line)
            sys.stdout.write (self.host_line)
            sys.stdout.write (self.acats_premable)

            # Output the main body.
            for name in self.tools:
                if name not in self.runs:
                    self.fatal (None, 'no results for ' + name)
                self.output_tool (self.runs[name])

            # Output the footer.
            if len (self.acats_failures) > 0:
                sys.stdout.write ('*** FAILURES: '
                                  + ' '.join (self.acats_failures) + '\n')
            sys.stdout.write (self.version_output)
            if self.end_line:
                sys.stdout.write (self.end_line[1])
        except IOError as e:
            self.fatal (e.filename, e.strerror)

Prog().main()
