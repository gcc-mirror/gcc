#!/bin/sh
# Copyright (C) 2006-2013 Free Software Foundation, Inc.
# This file is part of GCC.

# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# This script takes the following arguments:
#
#    - the target sysroot
#    - the value of $(MULTILIB_MATCHES)
#    - the value of $(MULTILIB_OPTIONS)
#
# It uses these arguments to construct a definition of SYSROOT_SUFFIX_SPEC,
# which it prints to the standard output.  For each multilib directory FOO,
# the script checks whether $sysroot has a subdirectory FOO, and if so will
# use /FOO for all compatible command-line options.  It will not add a
# suffix for /FOO's options otherwise.  These suffixes are concatenated,
# with one subspec for each space-separated entry in $(MULTILIB_OPTIONS).
set -e
sysroot=$1
matches=$2
options=$3

# For each multilib option OPT, add to $substs a sed command of the
# form "-e 's/OPT/OPT/'".
substs=""
for option in `echo "$options" | tr '/' ' '`
do
  substs="$substs -e 's/$option/$option/g'"
done

# For each ALIAS=CANONICAL entry in $MULTILIB_MATCHES, look for sed
# arguments in $substs of the form "-e 's/CANONICAL/.../'".  Replace
# such entries with "-e 's/CANONICAL/ALIAS|.../'".  Both the ALIAS and
# CANONICAL parts of $MULTILIB_MATCHES use '?' to stand for '='.
#
# After this loop, a command of the form "echo FOO | eval sed $substs"
# will replace a canonical option FOO with a %{...}-style spec pattern.
for match in $matches
do
  canonical=`echo "$match" | sed -e 's/=.*//' -e 's/?/=/g'`
  alias=`echo "$match" | sed -e 's/.*=//' -e 's/?/=/g'`
  substs=`echo "$substs" | sed -e "s,s/$canonical/,&$alias|,"`
done

# Build up the final SYSROOT_SUFFIX_SPEC in $spec.
spec=
for combo in $options
do
  # See which option alternatives in $combo have their own sysroot
  # directory.  Create a subspec of the form "%{PAT1:/DIR1;...;PATn:DIRn}"
  # from each such option OPTi, where DIRi is the directory associated
  # with OPTi and PATi is the result of passing OPTi through $substs.
  subspec=
  for option in `echo "$combo" | tr '/' ' '`
  do
    dir=`echo "$option" | sed 's/cpu=//'`
    if test -d "$sysroot/$dir"; then
      test -z "$subspec" || subspec="$subspec;"
      subspec="$subspec"`echo "$option" | eval sed $substs`":/$dir"
    fi
  done
  # Concatenate all the subspecs.
  test -z "$subspec" || spec="$spec%{$subspec}"
done
if test -n "$spec"; then
  echo "#undef SYSROOT_SUFFIX_SPEC"
  echo "#define SYSROOT_SUFFIX_SPEC \"$spec\""
fi
