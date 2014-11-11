#! /usr/bin/python
#
# Print a report on which libgccjit.so symbols are used in which test
# cases, and which lack test coverage.  Tested with Python 2.7 and 3.2
# To be run from the root directory of the source tree.
#
# Copyright (C) 2014 Free Software Foundation, Inc.
# Written by David Malcolm <dmalcolm@redhat.com>.
#
# This script is Free Software, and it can be copied, distributed and
# modified as defined in the GNU General Public License.  A copy of
# its license can be downloaded from http://www.gnu.org/copyleft/gpl.html

from collections import Counter
import glob
import re
import sys

def parse_map_file(path):
    """
    Parse libgccjit.map, returning the symbols in the API as a list of str.
    """
    syms = []
    with open(path) as f:
        for line in f:
            m = re.match('^\s+([a-z_]+);$', line)
            if m:
                syms.append(m.group(1))
    return syms

def parse_test_case(path):
    """
    Locate all symbol-like things in a C test case, yielding
    them as a sequence of str.
    """
    with open(path) as f:
        for line in f:
            for m in re.finditer('([_A-Za-z][_A-Za-z0-9]*)', line):
                yield m.group(1)

def find_test_cases():
    for path in glob.glob('gcc/testsuite/jit.dg/*.[ch]'):
        yield path

api_syms = parse_map_file('gcc/jit/libgccjit.map')

syms_in_test_cases = {}
for path in find_test_cases():
    syms_in_test_cases[path] = list(parse_test_case(path))

uses = Counter()
for sym in sorted(api_syms):
    print('symbol: %s' % sym)
    uses[sym] = 0
    for path in syms_in_test_cases:
        count = syms_in_test_cases[path].count(sym)
        uses[sym] += count
        if count:
            print('  uses in %s: %i' % (path, count))
    if uses[sym] == 0:
        print('  NEVER USED')
    sys.stdout.write('\n')

layout = '%40s  %5s  %s'
print(layout % ('SYMBOL', 'USES', 'HISTOGRAM'))
for sym, count in uses.most_common():
    print(layout % (sym, count, '*' * count if count else 'UNUSED'))
