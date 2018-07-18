#!/usr/bin/env python3

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

import sys
import os
import subprocess
import tempfile
import argparse

script_location = os.path.realpath(__file__)

parser = argparse.ArgumentParser()
parser.add_argument('location', metavar = 'dump_file',
    help = 'Location with SPEC benchmarks')
parser.add_argument('-s', '--sorting', dest = 'sorting',
    choices = ['branches', 'branch-hitrate', 'hitrate', 'coverage', 'name'],
    default = 'branches')
parser.add_argument('-d', '--def-file', help = 'path to predict.def')

args = parser.parse_args()

benchmarks = os.listdir(args.location)

for b in sorted(benchmarks):
    dumps = []
    for root, dirs, files in os.walk(os.path.join(args.location, b)):
        for x in files:
            if x.endswith('.profile'):
                dumps.append(os.path.join(root, x))

    if len(dumps) == 0:
        continue

    temp = tempfile.NamedTemporaryFile(delete = False)
    for d in dumps:
        temp.write(open(d, 'rb').read())

    temp.close()

    print()
    print(b)
    sys.stdout.flush()
    p = [os.path.join(os.path.dirname(script_location), 'analyze_brprob.py'),
        temp.name, '--sorting', args.sorting]
    if args.def_file != None:
        p += ['-d', args.def_file]

    p = subprocess.check_call(p)
    sys.stdout.flush()

    os.remove(temp.name)
