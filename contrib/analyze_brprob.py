#!/usr/bin/env python3
#
# Script to analyze results of our branch prediction heuristics
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
#
#
#
# This script is used to calculate two basic properties of the branch prediction
# heuristics - coverage and hitrate.  Coverage is number of executions
# of a given branch matched by the heuristics and hitrate is probability
# that once branch is predicted as taken it is really taken.
#
# These values are useful to determine the quality of given heuristics.
# Hitrate may be directly used in predict.def.
#
# Usage:
#  Step 1: Compile and profile your program.  You need to use -fprofile-generate
#    flag to get the profiles.
#  Step 2: Make a reference run of the intrumented application.
#  Step 3: Compile the program with collected profile and dump IPA profiles
#          (-fprofile-use -fdump-ipa-profile-details)
#  Step 4: Collect all generated dump files:
#          find . -name '*.profile' | xargs cat > dump_file
#  Step 5: Run the script:
#          ./analyze_brprob.py dump_file
#          and read results.  Basically the following table is printed:
#
# HEURISTICS                           BRANCHES  (REL)  HITRATE                COVERAGE  (REL)
# early return (on trees)                     3   0.2%  35.83% /  93.64%          66360   0.0%
# guess loop iv compare                       8   0.6%  53.35% /  53.73%       11183344   0.0%
# call                                       18   1.4%  31.95% /  69.95%       51880179   0.2%
# loop guard                                 23   1.8%  84.13% /  84.85%    13749065956  42.2%
# opcode values positive (on trees)          42   3.3%  15.71% /  84.81%     6771097902  20.8%
# opcode values nonequal (on trees)         226  17.6%  72.48% /  72.84%      844753864   2.6%
# loop exit                                 231  18.0%  86.97% /  86.98%     8952666897  27.5%
# loop iterations                           239  18.6%  91.10% /  91.10%     3062707264   9.4%
# DS theory                                 281  21.9%  82.08% /  83.39%     7787264075  23.9%
# no prediction                             293  22.9%  46.92% /  70.70%     2293267840   7.0%
# guessed loop iterations                   313  24.4%  76.41% /  76.41%    10782750177  33.1%
# first match                               708  55.2%  82.30% /  82.31%    22489588691  69.0%
# combined                                 1282 100.0%  79.76% /  81.75%    32570120606 100.0%
#
#
#  The heuristics called "first match" is a heuristics used by GCC branch
#  prediction pass and it predicts 55.2% branches correctly. As you can,
#  the heuristics has very good covertage (69.05%).  On the other hand,
#  "opcode values nonequal (on trees)" heuristics has good hirate, but poor
#  coverage.

import sys
import os
import re

def percentage(a, b):
    return 100.0 * a / b

class Summary:
    def __init__(self, name):
        self.name = name
        self.branches = 0
        self.count = 0
        self.hits = 0
        self.fits = 0

    def count_formatted(self):
        v = self.count
        for unit in ['','K','M','G','T','P','E','Z']:
            if v < 1000:
                return "%3.2f%s" % (v, unit)
            v /= 1000.0
        return "%.1f%s" % (v, 'Y')

class Profile:
    def __init__(self, filename):
        self.filename = filename
        self.heuristics = {}

    def add(self, name, prediction, count, hits):
        if not name in self.heuristics:
            self.heuristics[name] = Summary(name)

        s = self.heuristics[name]
        s.branches += 1
        s.count += count
        if prediction < 50:
            hits = count - hits
        s.hits += hits
        s.fits += max(hits, count - hits)

    def branches_max(self):
        return max([v.branches for k, v in self.heuristics.items()])

    def count_max(self):
        return max([v.count for k, v in self.heuristics.items()])

    def dump(self):
        print('%-36s %8s %6s  %-16s %14s %8s %6s' % ('HEURISTICS', 'BRANCHES', '(REL)',
              'HITRATE', 'COVERAGE', 'COVERAGE', '(REL)'))
        for (k, v) in sorted(self.heuristics.items(), key = lambda x: x[1].branches):
            print('%-36s %8i %5.1f%% %6.2f%% / %6.2f%% %14i %8s %5.1f%%' %
            (k, v.branches, percentage(v.branches, self.branches_max ()),
             percentage(v.hits, v.count), percentage(v.fits, v.count),
             v.count, v.count_formatted(), percentage(v.count, self.count_max()) ))

if len(sys.argv) != 2:
    print('Usage: ./analyze_brprob.py dump_file')
    exit(1)

profile = Profile(sys.argv[1])
r = re.compile('  (.*) heuristics: (.*)%.*exec ([0-9]*) hit ([0-9]*)')
for l in open(profile.filename).readlines():
    m = r.match(l)
    if m != None:
        name = m.group(1)
        prediction = float(m.group(2))
        count = int(m.group(3))
        hits = int(m.group(4))

        profile.add(name, prediction, count, hits)

profile.dump()
