#!/bin/sh

#  Test GCC.
#  Copyright (C) 1999, 2000, 2001, 2002, 2005, 2006  Free Software Foundation, Inc.

#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.

#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.

#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

# INPUT:
# btest <options> <target> <source> <prefix> <state> <build>

add_passes_despite_regression=0
dashj=''

# <options> can be
# --add-passes-despite-regression:
#  Add new "PASSes" despite there being some regressions.
# -j<n>:
#  Pass '-j<n>' to make.

case "$1" in
 --add-passes-despite-regression)
  add_passes_despite_regression=1; shift;;
 -j*)
  dashj=$1; shift;;
 -*) echo "Invalid option: $1"; exit 2;;
esac

# TARGET is the target triplet.  It should be the same one as used in
# constructing PREFIX.  Or it can be the keyword 'native', indicating
# a target of whatever platform the script is running on.
TARGET=$1
# SOURCE is the directory containing the toplevel configure.
SOURCE=$2

# PREFIX is the directory for the --prefix option to configure.
#   For cross compilers, it needs to contain header files,
#   libraries, and binutils.  PATH should probably include
#   $PREFIX/bin.  
PREFIX=$3
# This script also needs to include the GDB testsuite in
#   $PREFIX/share/gdb-testsuite.
GDB_TESTSUITE=$PREFIX/share/gdb-testsuite

# STATE is where the tester maintains its internal state,
#   described below.
STATE=$4

# BUILD is a temporary directory that this script will
#   delete and recreate, containing the build tree.
BUILD=$5

# you also probably need to set these variables:
# PATH: should contain a native gcc, and a cross gdb.
# DEJAGNU: should point to a site.exp suitable for testing
#   the compiler and debugger.


# OUTPUT: in $RESULT, one of the following keywords:
#   error	the script failed due to
#		a misconfiguration or resource limitation
#   build	the build failed
#   regress-<n>	the build succeeded, but there were <n>
#		testsuite regressions, listed in $REGRESS
#   pass	build succeeded and there were no regressions
RESULT=$STATE/RESULT
# in BUILD_LOG, the output of the build
BUILD_LOG=$STATE/build_log
# in FAILED, a list of failing testcases
FAILED=$STATE/failed
# in PASSES, the list of testcases we expect to pass
PASSES=$STATE/passes
# in REGRESS, a list of testcases we expected to pass but that failed
REGRESS=$STATE/regress

# Make sure various files exist.
[ -d $STATE ] || mkdir $STATE
[ -f $PASSES ] || touch $PASSES

# These lines should stay in this order, because
# that way if something is badly wrong and $RESULT can't
# be modified then cron will mail the error message.
# The reverse order could lead to the testsuite claiming that
# everything always passes, without running any tests.
echo error > $RESULT || exit 1
exec > $BUILD_LOG 2>&1 || exit 1

set -x

# Nuke $BUILD and recreate it.
rm -rf $BUILD $REGRESS $FAILED
mkdir $BUILD || exit 1
cd $BUILD || exit 1

H_BUILD=`$SOURCE/config.guess || exit 1`
H_HOST=$H_BUILD
if [ $TARGET = native ] ; then
  H_TARGET=$H_HOST
else
  H_TARGET=$TARGET
fi
H_REAL_TARGET=`$SOURCE/config.sub $H_TARGET || exit 1`

# TESTLOGS is the list of dejagnu .sum files that the tester should
# look at.
TESTLOGS="gcc/testsuite/gcc/gcc.sum
gcc/testsuite/g++/g++.sum
gcc/testsuite/gfortran/gfortran.sum
gcc/testsuite/objc/objc.sum"

# Build.
echo build > $RESULT
if [ $H_HOST = $H_TARGET ] ; then
  $SOURCE/configure --prefix=$PREFIX --target=$H_TARGET || exit 1
  if ! make $dashj bootstrap ; then
    [ -s .bad_compare ] || exit 1
    cat .bad_compare >> $REGRESS || exit 1
    make $dashj all || exit 1
  fi
else
  withopt="--with-gnu-ld --with-gnu-as"
  case "$H_TARGET" in
    *-linux*) ;;
    *) withopt="$withopt --with-newlib";;
  esac
  $SOURCE/configure --prefix=$PREFIX --target=$H_TARGET $withopt || exit 1
  make $dashj || exit 1
fi
echo error > $RESULT || exit 1

# Test GCC against its internal testsuite.
make $dashj -k check

if [ -f $BUILD/$H_TARGET/libstdc++-v3/testsuite/libstdc++.sum ] ; then
  TESTLOGS="$TESTLOGS $H_TARGET/libstdc++-v3/testsuite/libstdc++.sum"
fi

if [ -f $BUILD/$H_TARGET/libffi/testsuite/libffi.sum ] ; then
  TESTLOGS="$TESTLOGS $H_TARGET/libffi/testsuite/libffi.sum"
fi

if [ -f $BUILD/$H_TARGET/libjava/testsuite/libjava.sum ] ; then
  TESTLOGS="$TESTLOGS $H_TARGET/libjava/testsuite/libjava.sum"
fi

if [ -f $BUILD/$H_TARGET/libgomp/testsuite/libgomp.sum ] ; then
  TESTLOGS="$TESTLOGS $H_TARGET/libgomp/testsuite/libgomp.sum"
fi

# Test the just-built GCC with the GDB testsuite.
if [ -d $GDB_TESTSUITE ] ; then
  mkdir test-gdb || exit 1
  cd $GDB_TESTSUITE || exit 1
  for i in gdb.* ; do
    if [ -d $i ] ; then
      mkdir $BUILD/test-gdb/$i
    fi
  done
  cd $BUILD/test-gdb || exit 1
  echo "set host_alias $H_HOST" > site.exp
  echo "set host_triplet $H_HOST" >> site.exp
  echo "set target_alias $H_TARGET" >> site.exp
  echo "set target_triplet $H_REAL_TARGET" >> site.exp
  echo "set build_alias $H_BUILD" >> site.exp
  echo "set build_triplet $H_BUILD" >> site.exp
  echo "set srcdir $GDB_TESTSUITE" >> site.exp
  runtest --tool gdb
  TESTLOGS="$TESTLOGS test-gdb/gdb.sum"
fi

# Sanity-check the testlogs.  They should contain at least one PASS.
cd $BUILD || exit 1
for LOG in $TESTLOGS ; do
  if ! grep ^PASS: $LOG > /dev/null ; then
    echo build > $RESULT
    exit 1
  fi
done

# Work out what failed
for LOG in $TESTLOGS ; do
  L=`basename $LOG`
  awk '/^FAIL: / { print "'$L'",$2; }' $LOG || exit 1
done | sort | uniq > $FAILED || exit 1
comm -12 $FAILED $PASSES >> $REGRESS || exit 1
NUMREGRESS=`wc -l < $REGRESS | tr -d ' '`

if [ $NUMREGRESS -eq 0 ] || [ $add_passes_despite_regression -ne 0 ] ; then
  # Update the state.
  for LOG in $TESTLOGS ; do
    L=`basename $LOG`
    awk '/^PASS: / { print "'$L'",$2; }' $LOG || exit 1
  done | sort | uniq | comm -23 - $FAILED > ${PASSES}~ || exit 1
  [ -s ${PASSES}~ ] || exit 1
  if [ $NUMREGRESS -ne 0 ] ; then
    # The way we keep track of new PASSes when in "regress-N" for
    # --add-passes-despite-regression, is to *add* them to previous
    # PASSes.  Just as without this option, we don't forget *any* PASS
    # lines, because besides the ones in $REGRESS that we definitely
    # don't want to lose, their removal or rename may have been a
    # mistake (as in, the cause of the "regress-N" state).  If they
    # come back, we then know they're regressions.
    cat ${PASSES}~ ${PASSES} | sort -u > ${PASSES}~~
    mv ${PASSES}~~ ${PASSES} || exit 1
    rm ${PASSES}~ || exit 1
  else
    # In contrast to the merging for "regress-N", we just overwrite
    # the known PASSes when in the "pass" state, so we get rid of
    # stale PASS lines for removed, moved or otherwise changed tests
    # which may be added back with a different meaning later on.
    mv ${PASSES}~ ${PASSES} || exit 1
  fi
fi

if [ $NUMREGRESS -ne 0 ] ; then
  echo regress-$NUMREGRESS > $RESULT
  exit 1
fi

echo pass > $RESULT
exit 0
