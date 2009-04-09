#!/bin/sh

#  Build tools for testing GCC.
#  Copyright (C) 1999, 2000, 2001, 2002, 2009
#  Free Software Foundation, Inc.

#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 3 of the License, or
#  (at your option) any later version.

#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.

#  You should have received a copy of the GNU General Public License
#  along with this program; see the file COPYING3.  If not see
#  <http://www.gnu.org/licenses/>.

# INPUT:
# btest <target> <source> <prefix> <state> <build>
# TARGET is the target triplet.  It should be the same one as used in
# constructing PREFIX.  Or it can be the keyword 'native', indicating
# a target of whatever platform the script is running on.
TARGET=$1
# SOURCE is the directory containing the toplevel configure.
SOURCE=$2

# PREFIX is the directory for the --prefix option to configure.
PREFIX=$3

# STATE is where the tester maintains its internal state,
#   described below.
STATE=$4

# BUILD is a temporary directory that this script will
#   delete and recreate, containing the build tree.
BUILD=$5

# you also probably need to set these variables:
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

# TESTLOGS is the list of dejagnu .sum files that the tester should
# look at.
TESTLOGS="test/gcc/gcc.sum
test/g++/g++.sum"

# Nuke $BUILD and recreate it.
rm -rf $BUILD $REGRESS $FAILED
mkdir $BUILD $BUILD/build $BUILD/objs || exit 1
cd $BUILD || exit 1

# This script used to use config.guess, but that is not how releng
# determines hostnames.
H_BUILD=`$SOURCE/config.guess || exit 1`
H_HOST=$H_BUILD
if [ $TARGET = native ] ; then
  H_TARGET=$H_HOST
else
  H_TARGET=$TARGET
fi
H_REAL_TARGET=`$SOURCE/config.sub $H_TARGET || exit 1`
H_REAL_BUILD=`$SOURCE/config.sub $H_BUILD || exit 1`
H_REAL_HOST=`$SOURCE/config.sub $H_HOST || exit 1`

# Build.
echo build > $RESULT

cd $BUILD/build || exit 1
TMP_PREFIX=$BUILD/install
$SOURCE/configure --prefix=$PREFIX --target=$H_TARGET || exit 1
if [ $H_REAL_TARGET = $H_REAL_HOST -a $H_REAL_TARGET = i686-pc-linux-gnu ]
 then
  make all-gdb all-dejagnu all-ld || exit 1
  make install-gdb install-dejagnu install-ld || exit 1
elif [ $H_REAL_TARGET = $H_REAL_HOST ] ; then
  make bootstrap || exit 1
  make install || exit 1
else
  make || exit 1
  make install || exit 1
fi

if [ -x $PREFIX/bin/$TARGET-gdb ] ; then
  mkdir -p $PREFIX/share/gdb-testsuite || exit 1
  cd $SOURCE/gdb/testsuite || exit 1
  find . -print | cpio -pdmu $PREFIX/share/gdb-testsuite || exit 1
  # selftest.exp requires keeping old sources around, which is impractical
  rm $PREFIX/share/gdb-testsuite/gdb.base/selftest.exp
  # these tests seem to be broken and randomly failing
  rm -r $PREFIX/share/gdb-testsuite/gdb.mi
fi

echo pass > $RESULT
exit 0
