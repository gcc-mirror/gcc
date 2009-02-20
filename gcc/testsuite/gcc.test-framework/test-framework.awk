# Process the gcc.sum file for a run through gcc.test-framework.
# Print result lines that show potential problems.  Report the number
# of passing tests.
#
#
# Copyright (c) 2004, 2005, 2006, 2007, 2008, 2009
# Free Software Foundation, Inc.
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

function pass(msg)	{
			  passes++;
			# printf("pass   %s\n", $0);
			}
function fail(msg)	{
			  fails++;
			  printf("fail   %s\n", $0);
			}
function ignore(msg)	{
			# printf("ignore %s\n", $0);
			}

BEGIN			{ skip = 1; passes = 0; fails = 0; }
/Running.*test-frame/	{ skip = 0; next }
/gcc Summary/		{ skip = 1; next }
			{ if (skip) next }
/^$/			{ next }
# The post tests are always expected to pass.
/^PASS.*-2.c/		{ ignore(); next }
# dg-xfail-if applies to the compile step; these should be XPASS for the
# compile step on dox tests, which are run tests.
/^XPASS.*dox.*xiff.*-1.c.*\(test for excess errors\)/ { ignore(); next }
# xfail for scan-assembler-not tests doesn't apply to the compile step.
/^PASS.*sa.*-1.c.*\(test for excess errors\)/ { ignore(); next }
# ignore compile step, tests for warnings for output-exists[-not] tests.
/dg-outexists.*\(test for excess errors)/ { ignore(); next }
/dg-outexists.*\(test for warnings/ { ignore(); next }
/dg-outexists.*\(test for errors/ { ignore(); next }
# ignore compile step for dg-xfail-run-if tests.
/run-xrif.*\(test for excess errors)/ { ignore(); next }
# The other dox tests pass the compile step; ignore that message.
/^PASS.*dox.*\(test for excess errors\)/ { ignore(); next }
# The sf tests pass the compile step; ignore that message.
/^PASS.*sf.*\(test for excess errors\)/ { ignore(); next }
# Ignore passing compile step for scan tests.
/^PASS.*scan.*\(test for excess errors\)/ { ignore(); next }
# Ignore lines that begin with comma.
/^,/			{ ignore(); next }
# For tests of dg-output, ignore successful compilation.
/^PASS.*dg-output.*\(test for excess errors\)/	{ ignore(); next }
# For tests of dg-output, ignore successful execution.
/^PASS.*dg-output.*execution test/	{ ignore(); next }
/^PASS/			{ if (match ($0, "exp-P")) { pass(); next } }
/^FAIL/			{ if (match ($0, "exp-F")) { pass(); next } }
/^XPASS/		{ if (match ($0, "exp-XP")) { pass(); next } }
/^XFAIL/		{ if (match ($0, "exp-XF")) { pass(); next } }
/^UNSUPPORTED/		{ if (match ($0, "exp-U")) { pass(); next } }
			{ fail() }
END			{
			  printf("\n\t\t=== Test Framework Summary ===\n\n");
			  printf("# of expected passes\t\t%d\n", passes);
			  if (fails != 0)
			    printf("# of unexpected failures\t%d\n", fails);
			}
