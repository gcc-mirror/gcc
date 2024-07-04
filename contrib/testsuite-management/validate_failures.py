#!/usr/bin/env python3

# Script to compare testsuite failures against a list of known-to-fail
# tests.

# Contributed by Diego Novillo <dnovillo@google.com>
#
# Copyright (C) 2011-2024 Free Software Foundation, Inc.
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

"""This script provides a coarser XFAILing mechanism that requires no
detailed DejaGNU markings.  This is useful in a variety of scenarios:

- Development branches with many known failures waiting to be fixed.
- Release branches with known failures that are not considered
  important for the particular release criteria used in that branch.

The script must be executed from the toplevel build directory.  When
executed it will:

1- Determine the target built: TARGET
2- Determine the source directory: SRCDIR
3- Look for a failure manifest file in
   <SRCDIR>/<MANIFEST_SUBDIR>/<MANIFEST_NAME>.xfail
4- Collect all the <tool>.sum files from the build tree.
5- Produce a report stating:
   a- Failures expected in the manifest but not present in the build.
   b- Failures in the build not expected in the manifest.
6- If all the build failures are expected in the manifest, it exits
   with exit code 0.  Otherwise, it exits with error code 1.

Manifest files contain expected DejaGNU results that are otherwise
treated as failures.
They may also contain additional text:

# This is a comment.  - self explanatory
@include file         - the file is a path relative to the includer
@remove result text   - result text is removed from the expected set
"""

import datetime
import optparse
import os
import re
import sys

_VALID_TEST_RESULTS = [ 'FAIL', 'UNRESOLVED', 'XPASS', 'ERROR' ]
# <STATE>: <NAME> <DESCRIPTION"
_VALID_TEST_RESULTS_REX = re.compile('(%s):\s*(\S+)\s*(.*)'
                                     % "|".join(_VALID_TEST_RESULTS))

# Formats of .sum file sections
_TOOL_LINE_FORMAT = '\t\t=== %s tests ===\n'
_EXP_LINE_FORMAT = '\nRunning %s:%s ...\n'
_SUMMARY_LINE_FORMAT = '\n\t\t=== %s Summary ===\n'

# ... and their compiled regexs.
_TOOL_LINE_REX = re.compile('^\t\t=== (.*) tests ===\n')
# Match .exp file name, optionally prefixed by a "tool:" name and a
# path ending with "testsuite/"
_EXP_LINE_REX = re.compile('^Running (?:.*:)?(.*) \.\.\.\n')
_SUMMARY_LINE_REX = re.compile('^\t\t=== (.*) Summary ===\n')

# Subdirectory of srcdir in which to find the manifest file.
_MANIFEST_SUBDIR = 'contrib/testsuite-management'

# Pattern for naming manifest files.
# The first argument should be the toplevel GCC(/GNU tool) source directory.
# The second argument is the manifest subdir.
# The third argument is the manifest target, which defaults to the target
# triplet used during the build.
_MANIFEST_PATH_PATTERN = '%s/%s/%s.xfail'

# The options passed to the program.
_OPTIONS = None

def Error(msg):
  print('error: %s' % msg, file=sys.stderr)
  sys.exit(1)


class TestResult(object):
  """Describes a single DejaGNU test result as emitted in .sum files.

  We are only interested in representing unsuccessful tests.  So, only
  a subset of all the tests are loaded.

  The summary line used to build the test result should have this format:

  attrlist | XPASS: gcc.dg/unroll_1.c (test for excess errors)
  ^^^^^^^^   ^^^^^  ^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^^^
  optional   state  name              description
  attributes

  Attributes:
    attrlist: A comma separated list of attributes.
      Valid values:
        flaky            Indicates that this test may not always fail.  These
                         tests are reported, but their presence does not affect
                         the results.

        expire=YYYYMMDD  After this date, this test will produce an error
                         whether it is in the manifest or not.

    state: One of UNRESOLVED, XPASS or FAIL.
    name: File name for the test.
    description: String describing the test (flags used, dejagnu message, etc)
    ordinal: Monotonically increasing integer.
             It is used to keep results for one .exp file sorted
             by the order the tests were run.
    tool: Top-level testsuite name (aka "tool" in DejaGnu parlance) of the test.
    exp: Name of .exp testsuite file.
  """

  def __init__(self, summary_line, ordinal, tool, exp):
    try:
      (self.attrs, summary_line) = SplitAttributesFromSummaryLine(summary_line)
      try:
        (self.state,
         self.name,
         self.description) = _VALID_TEST_RESULTS_REX.match(summary_line).groups()
        if _OPTIONS.srcpath_regex and _OPTIONS.srcpath_regex != '':
          self.description = re.sub(_OPTIONS.srcpath_regex, '',
                                    self.description)
      except:
        print('Failed to parse summary line: "%s"' % summary_line,
              file=sys.stderr)
        raise
      self.ordinal = ordinal
      if tool == None or exp == None:
        # .sum file seem to be broken.  There was no "tool" and/or "exp"
        # lines preceding this result.
        print(f'.sum file seems to be broken: tool="{tool}", exp="{exp}", summary_line="{summary_line}"',
              file=sys.stderr)
        raise
      self.tool = tool
      self.exp = exp
    except ValueError:
      Error('Cannot parse summary line "%s"' % summary_line)

    if self.state not in _VALID_TEST_RESULTS:
      Error('Invalid test result %s in "%s" (parsed as "%s")' % (
            self.state, summary_line, self))

  def __lt__(self, other):
    if (self.tool != other.tool):
      return self.tool < other.tool
    if (self.exp != other.exp):
      return self.exp < other.exp
    if (self.name != other.name):
      return self.name < other.name
    return self.ordinal < other.ordinal

  def __hash__(self):
    return (hash(self.state) ^ hash(self.tool) ^ hash(self.exp)
            ^ hash(self.name) ^ hash(self.description))

  # Note that we don't include "attrs" in this comparison.  This means that
  # result entries "FAIL: test" and "flaky | FAIL: test" are considered
  # the same.  Therefore the ResultSet will preserve only the first occurence.
  # In practice this means that flaky entries should preceed expected fails
  # entries.
  def __eq__(self, other):
    return (self.state == other.state and
            self.tool == other.tool and
            self.exp == other.exp and
            self.name == other.name and
            self.description == other.description)

  def __ne__(self, other):
    return not (self == other)

  def __str__(self):
    attrs = ''
    if self.attrs:
      attrs = '%s | ' % self.attrs
    return '%s%s: %s %s' % (attrs, self.state, self.name, self.description)

  def ExpirationDate(self):
    # Return a datetime.date object with the expiration date for this
    # test result.  Return None, if no expiration has been set.
    if re.search(r'expire=', self.attrs):
      expiration = re.search(r'expire=(\d\d\d\d)(\d\d)(\d\d)', self.attrs)
      if not expiration:
        Error('Invalid expire= format in "%s".  Must be of the form '
              '"expire=YYYYMMDD"' % self)
      return datetime.date(int(expiration.group(1)),
                           int(expiration.group(2)),
                           int(expiration.group(3)))
    return None

  def HasExpired(self):
    # Return True if the expiration date of this result has passed.
    expiration_date = self.ExpirationDate()
    if expiration_date:
      return _OPTIONS.expiry_today_date > expiration_date


class ResultSet(set):
  """Describes a set of DejaGNU test results.
  This set can be read in from .sum files or emitted as a manifest.

  Attributes:
    current_tool: Name of the current top-level DejaGnu testsuite.
    current_exp: Name of the current .exp testsuite file.
    testsuites: A set of (tool, exp) tuples representing encountered testsuites.
  """

  def __init__(self):
    super().__init__()
    self.ResetToolExp()
    self.testsuites=set()

  def update(self, other):
    super().update(other)
    self.testsuites.update(other.testsuites)

  def ResetToolExp(self):
    self.current_tool = None
    self.current_exp = None

  def MakeTestResult(self, summary_line, ordinal=-1):
    return TestResult(summary_line, ordinal,
                      self.current_tool, self.current_exp)

  def Print(self, outfile=sys.stdout):
    current_tool = None
    current_exp = None

    for result in sorted(self):
      if current_tool != result.tool:
        current_tool = result.tool
        outfile.write(_TOOL_LINE_FORMAT % current_tool)
      if current_exp != result.exp:
        current_exp = result.exp
        outfile.write(_EXP_LINE_FORMAT % (current_tool, current_exp))
      outfile.write('%s\n' % result)

    outfile.write(_SUMMARY_LINE_FORMAT % 'Results')

  # Check if testsuite of expected_result is present in current results.
  # This is used to compare partial test results against a full manifest.
  def HasTestsuite(self, expected_result):
    return (expected_result.tool, expected_result.exp) in self.testsuites

def GetMakefileValue(makefile_name, value_name):
  if os.path.exists(makefile_name):
    makefile = open(makefile_name, encoding='latin-1', mode='r')
    for line in makefile:
      if line.startswith(value_name):
        (_, value) = line.split('=', 1)
        value = value.strip()
        makefile.close()
        return value
    makefile.close()
  return None


def ValidBuildDirectory(builddir):
  if (not os.path.exists(builddir) or
      not os.path.exists('%s/Makefile' % builddir)):
    return False
  return True


def IsComment(line):
  """Return True if line is a comment."""
  return line.startswith('#')


def SplitAttributesFromSummaryLine(line):
  """Splits off attributes from a summary line, if present."""
  if '|' in line and not _VALID_TEST_RESULTS_REX.match(line):
    (attrs, line) = line.split('|', 1)
    attrs = attrs.strip()
  else:
    attrs = ''
  line = line.strip()
  return (attrs, line)


def IsInterestingResult(result_set, line):
  """Return True if line is one of the summary lines we care about."""
  (_, line) = SplitAttributesFromSummaryLine(line)
  valid_result = bool(_VALID_TEST_RESULTS_REX.match(line))

  # If there's no tool defined it means that either the results section hasn't
  # started yet, or it is already over.
  if valid_result and result_set.current_tool is None:
    if _OPTIONS.verbosity >= 3:
      print(f'WARNING: Result "{line}" found outside sum file boundaries.',
            file=sys.stderr)
    return False

  return valid_result


def IsToolLine(line):
  """Return True if line mentions the tool (in DejaGnu terms) for the following tests."""
  return bool(_TOOL_LINE_REX.match(line))


def IsExpLine(line):
  """Return True if line mentions the .exp file for the following tests."""
  return bool(_EXP_LINE_REX.match(line))


def IsSummaryLine(line):
  """Return True if line starts .sum footer."""
  return bool(_SUMMARY_LINE_REX.match(line))


def IsInclude(line):
  """Return True if line is an include of another file."""
  return line.startswith("@include ")


def GetIncludeFile(line, includer):
  """Extract the name of the include file from line."""
  includer_dir = os.path.dirname(includer)
  include_file = line[len("@include "):]
  return os.path.join(includer_dir, include_file.strip())


def IsNegativeResult(line):
  """Return True if line should be removed from the expected results."""
  return line.startswith("@remove ")


def GetNegativeResult(line):
  """Extract the name of the negative result from line."""
  line = line[len("@remove "):]
  return line.strip()


def ParseManifestWorker(result_set, manifest_path):
  """Read manifest_path, adding the contents to result_set."""
  if _OPTIONS.verbosity >= 5:
    print('Parsing manifest file %s.' % manifest_path)
  manifest_file = open(manifest_path, encoding='latin-1', mode='r')
  for orig_line in manifest_file:
    line = orig_line.strip()
    if line == "":
      pass
    elif IsComment(line):
      pass
    elif IsNegativeResult(line):
      result_set.remove(result_set.MakeTestResult(GetNegativeResult(line)))
    elif IsInclude(line):
      ParseManifestWorker(result_set, GetIncludeFile(line, manifest_path))
    elif IsInterestingResult(result_set, line):
      result = result_set.MakeTestResult(line)
      if result.HasExpired():
        # Ignore expired manifest entries.
        if _OPTIONS.verbosity >= 4:
          print('WARNING: Expected failure "%s" has expired.' % line.strip())
        continue
      result_set.add(result)
    elif IsExpLine(orig_line):
      result_set.current_exp = _EXP_LINE_REX.match(orig_line).groups()[0]
      if _OPTIONS.srcpath_regex and _OPTIONS.srcpath_regex != '':
        result_set.current_exp = re.sub(_OPTIONS.srcpath_regex, '',
                                        result_set.current_exp)
    elif IsToolLine(orig_line):
      result_set.current_tool = _TOOL_LINE_REX.match(orig_line).groups()[0]
    elif IsSummaryLine(orig_line):
      result_set.ResetToolExp()
    else:
      Error('Unrecognized line in manifest file: %s' % line)
  manifest_file.close()


def ParseManifest(manifest_path):
  """Create a set of TestResult instances from the given manifest file."""
  result_set = ResultSet()
  ParseManifestWorker(result_set, manifest_path)
  return result_set


def ParseSummary(sum_fname):
  """Create a set of TestResult instances from the given summary file."""
  result_set = ResultSet()
  # ordinal is used when sorting the results so that tests within each
  # .exp file are kept sorted.
  ordinal=0
  sum_file = open(sum_fname, encoding='latin-1', mode='r')
  for line in sum_file:
    if IsInterestingResult(result_set, line):
      result = result_set.MakeTestResult(line, ordinal)
      ordinal += 1
      if result.HasExpired():
        # ??? What is the use-case for this?  How "expiry" annotations are
        # ??? supposed to be added to .sum results?
        # Tests that have expired are not added to the set of expected
        # results. If they are still present in the set of actual results,
        # they will cause an error to be reported.
        if _OPTIONS.verbosity >= 4:
          print('WARNING: Expected failure "%s" has expired.' % line.strip())
        continue
      result_set.add(result)
    elif IsExpLine(line):
      result_set.current_exp = _EXP_LINE_REX.match(line).groups()[0]
      if _OPTIONS.srcpath_regex and _OPTIONS.srcpath_regex != '':
        result_set.current_exp = re.sub(_OPTIONS.srcpath_regex, '',
                                        result_set.current_exp)
      result_set.testsuites.add((result_set.current_tool,
                                 result_set.current_exp))
    elif IsToolLine(line):
      result_set.current_tool = _TOOL_LINE_REX.match(line).groups()[0]
      result_set.current_exp = None
    elif IsSummaryLine(line):
      result_set.ResetToolExp()
  sum_file.close()
  return result_set


def GetManifest(manifest_path):
  """Build a set of expected failures from the manifest file.

  Each entry in the manifest file should have the format understood
  by the TestResult constructor.

  If no manifest file exists for this target, it returns an empty set.
  """
  if os.path.exists(manifest_path):
    return ParseManifest(manifest_path)
  else:
    return ResultSet()


def CollectSumFiles(builddir):
  sum_files = []
  for root, dirs, files in os.walk(builddir):
    for ignored in ('.svn', '.git'):
      if ignored in dirs:
        dirs.remove(ignored)
    for fname in files:
      if fname.endswith('.sum'):
        sum_files.append(os.path.join(root, fname))
  return sum_files


def GetResults(sum_files, build_results = None):
  """Collect all the test results from the given .sum files."""
  if build_results == None:
    build_results = ResultSet()
  for sum_fname in sum_files:
    if _OPTIONS.verbosity >= 3:
      print('\t%s' % sum_fname)
    build_results.update(ParseSummary(sum_fname))
  return build_results


def CompareResults(manifest, actual):
  """Compare sets of results and return two lists:
     - List of results present in ACTUAL but missing from MANIFEST.
     - List of results present in MANIFEST but missing from ACTUAL.
  """
  # Collect all the actual results not present in the manifest.
  # Results in this set will be reported as errors.
  actual_vs_manifest = ResultSet()
  for actual_result in actual:
    if actual_result not in manifest:
      actual_vs_manifest.add(actual_result)

  # Collect all the tests in the manifest that were not found
  # in the actual results.
  # Results in this set will be reported as warnings (since
  # they are expected failures that are not failing anymore).
  manifest_vs_actual = ResultSet()
  for expected_result in manifest:
    # Ignore tests marked flaky.
    if 'flaky' in expected_result.attrs:
      continue
    # We try to support comparing partial results vs full manifest
    # (e.g., manifest has failures for gcc, g++, gfortran, but we ran only
    # g++ testsuite).  To achieve this we record encountered testsuites in
    # actual.testsuites set, and then we check it here using HasTestsuite().
    if expected_result not in actual and actual.HasTestsuite(expected_result):
      manifest_vs_actual.add(expected_result)

  return actual_vs_manifest, manifest_vs_actual


def GetManifestPath(user_provided_must_exist):
  """Return the full path to the manifest file."""
  manifest_path = _OPTIONS.manifest
  if manifest_path:
    if user_provided_must_exist and not os.path.exists(manifest_path):
      Error('Manifest does not exist: %s' % manifest_path)
    return manifest_path
  else:
    (srcdir, target) = GetBuildData()
    if not srcdir:
      Error('Could not determine the location of GCC\'s source tree. '
            'The Makefile does not contain a definition for "srcdir".')
    if not target:
      Error('Could not determine the target triplet for this build. '
            'The Makefile does not contain a definition for "target_alias".')
    return _MANIFEST_PATH_PATTERN % (srcdir, _MANIFEST_SUBDIR, target)


def GetBuildData():
  if not ValidBuildDirectory(_OPTIONS.build_dir):
    # If we have been given a set of results to use, we may
    # not be inside a valid GCC build directory.  In that case,
    # the user must provide both a manifest file and a set
    # of results to check against it.
    if not _OPTIONS.results or not _OPTIONS.manifest:
      Error('%s is not a valid GCC top level build directory. '
            'You must use --manifest and --results to do the validation.' %
            _OPTIONS.build_dir)
    else:
      return None, None
  srcdir = GetMakefileValue('%s/Makefile' % _OPTIONS.build_dir, 'srcdir =')
  target = GetMakefileValue('%s/Makefile' % _OPTIONS.build_dir, 'target_alias=')
  if _OPTIONS.verbosity >= 3:
    print('Source directory: %s' % srcdir)
    print('Build target:     %s' % target)
  return srcdir, target


def PrintSummary(summary):
  summary.Print()

def GetSumFiles(results, build_dir):
  if not results:
    if _OPTIONS.verbosity >= 3:
      print('Getting actual results from build directory %s' % build_dir)
    sum_files = CollectSumFiles(build_dir)
  else:
    if _OPTIONS.verbosity >= 3:
      print('Getting actual results from user-provided results')
    sum_files = results.split()
  return sum_files


def PerformComparison(expected, actual):
  actual_vs_expected, expected_vs_actual = CompareResults(expected, actual)

  if _OPTIONS.inverse_match:
    # Switch results if inverse comparison is requested.
    # This is useful in detecting flaky tests that FAILed in expected set,
    # but PASSed in actual set.
    actual_vs_expected, expected_vs_actual \
      = expected_vs_actual, actual_vs_expected

  tests_ok = True
  if len(actual_vs_expected) > 0:
    if _OPTIONS.verbosity >= 3:
      print('\n\nUnexpected results in this build (new failures)')
    if _OPTIONS.verbosity >= 1:
      PrintSummary(actual_vs_expected)
    tests_ok = False

  if _OPTIONS.verbosity >= 2 and len(expected_vs_actual) > 0:
    print('\n\nExpected results not present in this build (fixed tests)'
          '\n\nNOTE: This is not a failure.  It just means that these '
          'tests were expected\nto fail, but either they worked in '
          'this configuration or they were not\npresent at all.\n')
    PrintSummary(expected_vs_actual)

  if tests_ok and _OPTIONS.verbosity >= 3:
    print('\nSUCCESS: No unexpected failures.')

  return tests_ok


def CheckExpectedResults():
  manifest_path = GetManifestPath(True)
  if _OPTIONS.verbosity >= 3:
    print('Manifest:         %s' % manifest_path)
  manifest = GetManifest(manifest_path)
  sum_files = GetSumFiles(_OPTIONS.results, _OPTIONS.build_dir)
  actual = GetResults(sum_files)

  if _OPTIONS.verbosity >= 5:
    print('\n\nTests expected to fail')
    PrintSummary(manifest)
    print('\n\nActual test results')
    PrintSummary(actual)

  return PerformComparison(manifest, actual)


def ProduceManifest():
  manifest_path = GetManifestPath(False)
  if _OPTIONS.verbosity >= 3:
    print('Manifest:         %s' % manifest_path)
  if os.path.exists(manifest_path) and not _OPTIONS.force:
    Error('Manifest file %s already exists.\nUse --force to overwrite.' %
          manifest_path)

  sum_files = GetSumFiles(_OPTIONS.results, _OPTIONS.build_dir)
  actual = GetResults(sum_files)
  manifest_file = open(manifest_path, encoding='latin-1', mode='w')
  actual.Print(manifest_file)
  actual.Print()
  manifest_file.close()

  return True


def CompareBuilds():
  sum_files = GetSumFiles(_OPTIONS.results, _OPTIONS.build_dir)
  actual = GetResults(sum_files)

  clean = ResultSet()

  if _OPTIONS.manifest:
    manifest_path = GetManifestPath(True)
    if _OPTIONS.verbosity >= 3:
      print('Manifest:         %s' % manifest_path)
    clean = GetManifest(manifest_path)

  clean_sum_files = GetSumFiles(_OPTIONS.results, _OPTIONS.clean_build)
  clean = GetResults(clean_sum_files, clean)

  return PerformComparison(clean, actual)


def Main(argv):
  parser = optparse.OptionParser(usage=__doc__)

  # Keep the following list sorted by option name.
  parser.add_option('--build_dir', action='store', type='string',
                    dest='build_dir', default='.',
                    help='Build directory to check (default = .)')
  parser.add_option('--clean_build', action='store', type='string',
                    dest='clean_build', default=None,
                    help='Compare test results from this build against '
                    'those of another (clean) build.  Use this option '
                    'when comparing the test results of your patch versus '
                    'the test results of a clean build without your patch. '
                    'You must provide the path to the top directory of your '
                    'clean build.')
  parser.add_option('--force', action='store_true', dest='force',
                    default=False, help='When used with --produce_manifest, '
                    'it will overwrite an existing manifest file '
                    '(default = False)')
  parser.add_option('--expiry_date', action='store',
                    dest='expiry_today_date', default=None,
                    help='Use provided date YYYYMMDD to decide whether '
                    'manifest entries with expiry settings have expired '
                    'or not. (default = Use today date)')
  parser.add_option('--srcpath', action='store', type='string',
                    dest='srcpath_regex', default='[^ ]+/testsuite/',
                    help='Remove provided path (can be a regex) from '
                    'the result entries.  This is useful to remove '
                    'occasional filesystem path from the results. '
                    '(default = "[^ ]+/testsuite/")')
  parser.add_option('--inverse_match', action='store_true',
                    dest='inverse_match', default=False,
                    help='Inverse result sets in comparison. '
                    'Output unexpected passes as unexpected failures and '
                    'unexpected failures as unexpected passes. '
                    'This is used to catch FAIL->PASS flaky tests. '
                    '(default = False)')
  parser.add_option('--manifest', action='store', type='string',
                    dest='manifest', default=None,
                    help='Name of the manifest file to use (default = '
                    'taken from '
                    'contrib/testsuite-managment/<target_alias>.xfail)')
  parser.add_option('--produce_manifest', action='store_true',
                    dest='produce_manifest', default=False,
                    help='Produce the manifest for the current '
                    'build (default = False)')
  parser.add_option('--results', action='store', type='string',
                    dest='results', default=None, help='Space-separated list '
                    'of .sum files with the testing results to check. The '
                    'only content needed from these files are the lines '
                    'starting with FAIL, XPASS or UNRESOLVED (default = '
                    '.sum files collected from the build directory).')
  parser.add_option('--verbosity', action='store', dest='verbosity',
                    type='int', default=3, help='Verbosity level '
                    '(default = 3). Level 0: only error output, this is '
                    'useful in scripting when only the exit code is used. '
                    'Level 1: output unexpected failures. '
                    'Level 2: output unexpected passes. '
                    'Level 3: output helpful information. '
                    'Level 4: output notification on expired entries. '
                    'Level 5: output debug information.')
  global _OPTIONS
  (_OPTIONS, _) = parser.parse_args(argv[1:])

  # Set "today" date to compare expiration entries against.
  # Setting expiration date into the future allows re-detection of flaky
  # tests and creating fresh entries for them before the current flaky entries
  # expire.
  if _OPTIONS.expiry_today_date:
    today_date = re.search(r'(\d\d\d\d)(\d\d)(\d\d)',
                           _OPTIONS.expiry_today_date)
    if not today_date:
        Error('Invalid --expiry_today_date format "%s".  Must be of the form '
              '"expire=YYYYMMDD"' % _OPTIONS.expiry_today_date)
    _OPTIONS.expiry_today_date=datetime.date(int(today_date.group(1)),
                                             int(today_date.group(2)),
                                             int(today_date.group(3)))
  else:
    _OPTIONS.expiry_today_date = datetime.date.today()

  if _OPTIONS.produce_manifest:
    retval = ProduceManifest()
  elif _OPTIONS.clean_build:
    retval = CompareBuilds()
  else:
    retval = CheckExpectedResults()

  if retval:
    return 0
  else:
    return 2


if __name__ == '__main__':
  retval = Main(sys.argv)
  sys.exit(retval)
