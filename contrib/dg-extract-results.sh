#! /bin/sh

# For a specified tool and optional list of test variants, extract
# test results from one or more test summary (.sum) files and combine
# the results into a new test summary file, sent to the standard output.
# The resulting file can be used with test result comparison scripts for
# results from tests that were run in parallel.  See usage() below.

# Copyright (C) 2008 Free Software Foundation
# Contributed by Janis Johnson <janis187@us.ibm.com>
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

PROGNAME=dg-extract-results.sh

usage() {
  cat <<EOF >&2
Usage: $PROGNAME [-t tool] [-l variant-list] [-L] sum-file ...

    tool           The tool (e.g. g++, libffi) for which to create a
                   new test summary file.  If not specified then all
                   specified sum files must be for the same tool.
    variant-list   One or more test variant names.  If the list is
                   not specified then one is constructed from all
                   variants in the files for <tool>.
    sum-file       A test summary file with the format of those
                   created by runtest from DejaGnu.
    If -L is used, merge *.log files instead of *.sum.  In this
    mode the exact order of lines may not be preserved, just different
    Running *.exp chunks should be in correct order.
EOF
}

# Write a message to the standard error.

msg() {
  echo "$@" >&2
}

# Parse the command-line options.

VARIANTS=""
TOOL=""
MODE="sum"

while getopts "l:t:L" ARG; do
  case $ARG in
  l)  VARIANTS="${VARIANTS} ${OPTARG}";;
  t)  test -z "$TOOL" || (msg "${PROGNAME}: only one tool can be specified"; exit 1);
      TOOL="${OPTARG}";;
  L)  MODE="log";;
  \?) usage; exit 0;;
  esac
done
shift `expr ${OPTIND} - 1`

if test $# -lt 1 ; then
  usage
  exit 1
fi

TMPDIR=${TMPDIR-/tmp}
SUM_FILES="$@"
FIRST_SUM=$1
TMP=
trap 'EXIT_STATUS=$?; rm -rf $TMP && exit $EXIT_STATUS' 0
# Create a (secure) tmp directory for tmp files.
{
  TMP=`(umask 077 && mktemp -d -q "${TMPDIR}/dg-combine-results-$$-XXXXXX") 2>/dev/null` &&
  test -n "$TMP" && test -d "$TMP"
} ||
{
  TMP=${TMPDIR}/dg-combine-results-$$-$RANDOM
  (umask 077 && mkdir $TMP)
} ||
{
  msg "${PROGNAME}: cannot create a temporary directory"
  { (exit 1); exit 1; }
}

# Find a good awk.

if test -z "$AWK" ; then
  for AWK in gawk nawk awk
  do
    if type $AWK 2>&1 | grep 'not found' > /dev/null 2>&1 ; then
      :
    else
      break
    fi
  done
fi

# Verify that the specified summary files exist.

ERROR=0
for FILE in $SUM_FILES
do
  if ! test -f $FILE ; then
    msg "${PROGNAME}: file $FILE does not exist."
    ERROR=1
  fi
done
test $ERROR -eq 0 || exit 1

if [ -z "$TOOL" ]; then
  # If no tool was specified, all specified summary files must be for
  # the same tool.

  CNT=`grep '=== .* tests ===' $SUM_FILES | $AWK '{ print $3 }' | sort -u | wc -l`
  if [ $CNT -eq 1 ]; then
    TOOL=`grep '=== .* tests ===' $FIRST_SUM | $AWK '{ print $2 }'`
  else
    msg "${PROGNAME}: sum files are for multiple tools, specify a tool"
    msg ""
    usage
    exit 1
  fi
else
  # Ignore the specified summary files that are not for this tool.  This
  # should keep the relevant files in the same order.

  SUM_FILES=`grep -l "=== $TOOL" $SUM_FILES`
  if test -z "$SUM_FILES" ; then
    msg "${PROGNAME}: none of the specified files are results for $TOOL"
    exit 1
  fi
fi

if [ "$TOOL" = acats ]; then
  # Acats *.sum or *.log files aren't dejagnu generated, and they have
  # somewhat different format.
  ACATS_AWK=${TMP}/acats.awk
  cat <<EOF > $ACATS_AWK
BEGIN {
  print_prologue=1; chapter=""; insummary=0
  passcnt=0; failcnt=0; unsupcnt=0; failures=""
}
/=== acats configuration ===/ {
  insummary=0
  if (print_prologue) print
  next
}
/=== acats tests ===/ {
  if (print_prologue) print
  print_prologue=0
  next
}
/^Running chapter / {
  chapter=\$3
  print > "${TMP}/chapter-"chapter
  next
}
/=== acats Summary ===/ {
  chapter=""
  insummary=1
  next
}
/^# of expected passes/		{ if (insummary == 1) passcnt += \$5; next; }
/^# of unexpected failures/	{ if (insummary == 1) failcnt += \$5; next; }
/^# of unsupported tests/	{ if (insummary == 1) unsupcnt += \$5; next; }
/^\*\*\* FAILURES: / {
  if (insummary == 1) {
    if (failures) sub(/^\*\*\* FAILURES:/,"")
    failures=failures""\$0
  }
}
{
  if (print_prologue) { print; next }
  if (chapter) print > "${TMP}/chapter-"chapter
}
END {
  system ("cat ${TMP}/chapter-*")
  print "		=== acats Summary ==="
  print "# of expected passes		" passcnt
  print "# of unexpected failures	" failcnt
  if (unsupcnt) print "# of unsupported tests		" unsupcnt
  if (failures) print failures
}
EOF

  $AWK -f $ACATS_AWK $SUM_FILES
  exit 0
fi

# If no variants were specified, find all variants in the remaining
# summary files.  Otherwise, ignore specified variants that aren't in
# any of those summary files.

if test -z "$VARIANTS" ; then
  VAR_AWK=${TMP}/variants.awk
  cat <<EOF > $VAR_AWK
/^Schedule of variations:/      { in_vars=1; next }
/^$/                            { in_vars=0 }
/^Running target/               { exit }
{ if (in_vars==1) print \$1; else next }
EOF

  touch ${TMP}/varlist
  for FILE in $SUM_FILES; do
    $AWK -f $VAR_AWK $FILE >> ${TMP}/varlist
  done
  VARIANTS="`sort -u ${TMP}/varlist`"
else
  VARS="$VARIANTS"
  VARIANTS=""
  for VAR in $VARS
  do
    grep -q "Running target $VAR" $SUM_FILES && VARIANTS="$VARIANTS $VAR"
  done
fi

# Find out if we have more than one variant, or any at all.

VARIANT_COUNT=0
for VAR in $VARIANTS
do
  VARIANT_COUNT=`expr $VARIANT_COUNT + 1`
done

if test $VARIANT_COUNT -eq 0 ; then
  msg "${PROGNAME}: no file for $TOOL has results for the specified variants"
  exit 1
fi

cat $SUM_FILES \
  | $AWK '/^Running/ { if ($2 != "target" && $3 == "...") print "EXPFILE: "$2 } ' \
  | sort -u > ${TMP}/expfiles

# Write the begining of the combined summary file.

head -n 2 $FIRST_SUM
echo
echo "		=== $TOOL tests ==="
echo
echo "Schedule of variations:"
for VAR in $VARIANTS
do
  echo "    $VAR"
done
echo

# For each test variant for the tool, copy test reports from each of the
# summary files.  Set up two awk scripts from within the loop to
# initialize VAR and TOOL with the script, rather than assuming that the
# available version of awk can pass variables from the command line.

for VAR in $VARIANTS
do
  GUTS_AWK=${TMP}/guts.awk
  cat << EOF > $GUTS_AWK
BEGIN {
  variant="$VAR"
  firstvar=1
  expfileno=1
  cnt=0
  print_using=0
}
/^EXPFILE: / {
  expfiles[expfileno] = \$2
  expfilesr[\$2] = expfileno
  expfileno = expfileno + 1
}
/^Running target / {
  curvar = \$3
  if (variant == curvar && firstvar == 1) { print; print_using=1; firstvar = 0 }
  next
}
/^Using / {
  if (variant == curvar && print_using) { print; next }
}
/^Running / {
  print_using=0
  if (variant == curvar) {
    curfile="${TMP}/list"expfilesr[\$2]
    expfileseen[\$2]=expfileseen[\$2] + 1
    testname="00"
    next
  }
}
/\===/ { curvar = ""; next }
/^(PASS|XPASS|FAIL|XFAIL|UNRESOLVED|WARNING|ERROR|UNSUPPORTED|UNTESTED|KFAIL):/ {
  testname=\$2
  # Ugly hack for gfortran.dg/dg.exp
  if ("$TOOL" == "gfortran" && testname ~ /^gfortran.dg\/g77\//)
    testname="h"testname
}
/^$/ { if ("$MODE" == "sum") next }
{ if (variant == curvar && curfile) {
    if ("$MODE" == "sum") {
      printf "%s %08d|", testname, cnt > curfile
      cnt = cnt + 1
    }
    filewritten[curfile]=1
    print > curfile
  } else
    next
}
END {
  n=1
  while (n < expfileno) {
    if (expfileseen[expfiles[n]]) {
      print "Running "expfiles[n]" ..."
      if (filewritten["${TMP}/list"n]) {
	if (expfileseen[expfiles[n]] == 1)
	  cmd="cat"
	else
	  cmd="LC_ALL=C sort"
	if ("$MODE" == "sum")
	  system (cmd" ${TMP}/list"n" | sed -n 's/^[^ ]* [^ |]*|//p'")
	else
	  system ("cat ${TMP}/list"n)
      }
    }
    n = n + 1
  }
}
EOF

  SUMS_AWK=${TMP}/sums.awk
  rm -f $SUMS_AWK
  cat << EOF > $SUMS_AWK
BEGIN {
  variant="$VAR"
  tool="$TOOL"
  passcnt=0; failcnt=0; untstcnt=0; xpasscnt=0; xfailcnt=0; unsupcnt=0; unrescnt=0;
  curvar=""; insummary=0
}
/^Running target /		{ curvar = \$3; next }
/^# of /			{ if (variant == curvar) insummary = 1 }
/^# of expected passes/		{ if (insummary == 1) passcnt += \$5; next; }
/^# of unexpected successes/	{ if (insummary == 1) xpasscnt += \$5; next; }
/^# of unexpected failures/	{ if (insummary == 1) failcnt += \$5; next; }
/^# of expected failures/	{ if (insummary == 1) xfailcnt += \$5; next; }
/^# of untested testcases/	{ if (insummary == 1) untstcnt += \$5; next; }
/^# of unresolved testcases/	{ if (insummary == 1) unrescnt += \$5; next; }
/^# of unsupported tests/	{ if (insummary == 1) unsupcnt += \$5; next; }
/^$/				{ if (insummary == 1)
				    { insummary = 0; curvar = "" }
				  next
				}
{ next }
END {
  printf ("\t\t=== %s Summary for %s ===\n\n", tool, variant)
  if (passcnt != 0) printf ("# of expected passes\t\t%d\n", passcnt)
  if (xpasscnt != 0) printf ("# of unexpected successes\t%d\n", xpasscnt)
  if (failcnt != 0) printf ("# of unexpected failures\t%d\n", failcnt)
  if (xfailcnt != 0) printf ("# of expected failures\t\t%d\n", xfailcnt)
  if (untstcnt != 0) printf ("# of untested testcases\t\t%d\n", untstcnt)
  if (unrescnt != 0) printf ("# of unresolved testcases\t%d\n", unrescnt)
  if (unsupcnt != 0) printf ("# of unsupported tests\t\t%d\n", unsupcnt)
}
EOF

  PVAR=`echo $VAR | sed 's,/,.,g'`
  TMPFILE=${TMP}/var-$PVAR
  rm -f $TMPFILE
  rm -f ${TMP}/list*
  cat ${TMP}/expfiles $SUM_FILES | $AWK -f $GUTS_AWK
  cat $SUM_FILES | $AWK -f $SUMS_AWK > $TMPFILE
  # If there are multiple variants, output the counts for this one;
  # otherwise there will just be the final counts at the end.
  test $VARIANT_COUNT -eq 1 || cat $TMPFILE
done

# Set up an awk script to get the combined summary counts for the tool.

TOTAL_AWK=${TMP}/total.awk
cat << EOF > $TOTAL_AWK
BEGIN {
  tool="$TOOL"
  passcnt=0; failcnt=0; untstcnt=0; xpasscnt=0; xfailcnt=0; unsupcnt=0; unrescnt=0
}
/^# of expected passes/		{ passcnt += \$5 }
/^# of unexpected failures/	{ failcnt += \$5 }
/^# of unexpected successes/	{ xpasscnt += \$5 }
/^# of expected failures/	{ xfailcnt += \$5 }
/^# of untested testcases/	{ untstcnt += \$5 }
/^# of unresolved testcases/	{ unrescnt += \$5 }
/^# of unsupported tests/	{ unsupcnt += \$5 }
END {
  printf ("\n\t\t=== %s Summary ===\n\n", tool)
  if (passcnt != 0) printf ("# of expected passes\t\t%d\n", passcnt)
  if (failcnt != 0) printf ("# of unexpected failures\t%d\n", failcnt)
  if (xpasscnt != 0) printf ("# of unexpected successes\t%d\n", xpasscnt)
  if (xfailcnt != 0) printf ("# of expected failures\t\t%d\n", xfailcnt)
  if (untstcnt != 0) printf ("# of untested testcases\t\t%d\n", untstcnt)
  if (unrescnt != 0) printf ("# of unresolved testcases\t%d\n", unrescnt)
  if (unsupcnt != 0) printf ("# of unsupported tests\t\t%d\n", unsupcnt)
}
EOF

# Find the total summaries for the tool and add to the end of the output.
cat ${TMP}/var-* | $AWK -f $TOTAL_AWK

# This is ugly, but if there's version output from the compiler under test
# at the end of the file, we want it.  The other thing that might be there
# is the final summary counts.
tail -n 2 $FIRST_SUM | grep -q '^#' || tail -n 2 $FIRST_SUM

exit 0
