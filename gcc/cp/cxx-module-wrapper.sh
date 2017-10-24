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

shopt -s extglob nullglob

progname=${0##*/}
module=$1
bmi=$2
source=$3
importer=$4

args=$(eval echo $COLLECT_GCC_OPTIONS)
gcc="$COLLECT_GCC"

module=${bmi%.nms}
for root in . ${CXX_MODULE_PATH//:/\ } ; do
  src=$(echo $root/$module.@(cc|C|ccm))
  test $src && break
done
src=${src#./}
if test $src ; then
    echo "$progname: note: Compiling module interface $module ($src)" >&2
    case " $args " in
	(' -v ') set -x ;;
    esac
    exec $gcc $args -c -fmodule-root=$root -fmodule-output=$root/$bmi $src
fi
echo "$progname: cannot find source for module $module" >&2
exit 1
