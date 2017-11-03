#! /bin/sh

# Wrapper to rebuild a missing C++ module BMI

# Copyright (C) 2017 Free Software Foundation, Inc.
# Written by Nathan Sidwell <nathan@acm.org> while at FaceBook

#This file is part of GCC.

#GCC is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 3, or (at your option)
#any later version.

#GCC is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with GCC; see the file COPYING3.  If not see
#<http://www.gnu.org/licenses/>.

# Eventually this should be a portable compiled program, like
# lto-wrapper.

# FIXME this is a Quick and Dirty Hack.  It is not robust
# The API is not stable and can change at a moment's notice.
# It'd be nice to inform the driver that we're compiling
# an additional file, so it doesn't compile it itself
# It'd also be nice to do some kind of locking so parallel makes 'just work'

shopt -s extglob nullglob

progname=${0##*/}
if test "$#" -ne 4 ; then
  echo "usage: ${progname} module-name bmi-file original-source importing-file" >&2
  exit 1
fi

module=$1
bmi=$2
source=$3
importer=$4

# If we're inside make and there's a Makefile, just invoke make for the bmi.
if test ${MAKELEVEL:=0} -gt 0 -a -e Makefile ; then
  exec make ${MAKEFLAGS} $bmi
fi

verbose=false
ign=false
cmd="$COLLECT_GCC"
for arg in $(eval echo $COLLECT_GCC_OPTIONS)
do
  $ign || case "$arg" in
    ('-v') verbose=true ;;
    ('-S') ign=true ;;
    ('-c') ign=true ;;
    ('-o') ign=true ;;
    (*)  ;;
  esac
  $ign || cmd+=" $arg"
  test "$arg" = '-o' || ign=false
done

module=${bmi%.nms}
for root in $(dirname $source) . ${CXX_MODULE_PATH//:/\ } ; do
  src=$(echo $root/$module.@(cc|C|ccm))
  test $src && break
done
src=${src#./}
if test $src ; then
    echo "$progname: note: compiling module interface $module ($src)" >&2
    cmd+=" -fmodule-output=$bmi -c $src"
    $verbose && set -x
    exec $cmd
fi
echo "$progname: cannot find source for module $module" >&2
exit 1
