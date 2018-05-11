#!/bin/bash

# Copyright (C) 2018 Free Software Foundation, Inc.
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

progname=${0##*/}
main=
verbose=false
shopt -s extglob nullglob

compile () {
    local module=$1
    local ign=false
    local cmd="$COLLECT_GCC"
    local action=-c
    local arg
    for arg in $(eval echo $COLLECT_GCC_OPTIONS)
    do
	$ign || case "$arg" in
	    (-S) action=-S ; ign=true ;;
	    (-c) ign=true ;;
	    (-o) ign=true ;;
	    (-fmodule-output=*) ign=true ;;
	    (*)  ;;
	esac
	$ign || cmd+=" $arg"
	test "$arg" = '-o' || ign=false
    done

    # look for something named by the module name
    src=$(echo $(dirname $main)/$module.@(cc|C|ccm))
    # dirname could have created ./ prefix
    src="${src#./}"

    if test -z "$src" -o ! -e "$src" ; then
	return 1
    fi

    $verbose && echo "$progname: compiling module interface $module ($src)" >&2
    cmd+=" $action $src"
    $cmd
}

while test "$#" != 0 ; do
    case "$1" in
	(-v) verbose=true ;;
	(-*) echo "Unknown option '$1'" >&2 ; exit 1 ;;
	(*) break ;;
    esac
    shift
done

while read cmd arg0 arg1 ; do
    $verbose && echo "$progname< $cmd $arg0 $arg1" >&2
    resp=
    case "$cmd" in
	(HELO)
	    if test $arg0 = 0 ; then
		main="$arg1"
		resp="200 OK"
	    else
		resp="520 Bad Version"
	    fi
	    ;;
	(IMPT)
	    file=$(echo $arg0 | tr . -)
	    if test -e $file.nms || compile $file ; then
		resp="250 $arg0 $file.nms"
	    else
		resp="550 $arg0"
	    fi
	    ;;
	(EXPT)
	    file=$(echo $arg0 | tr . -)
	    resp="250 $arg0 $file.nms"
	    ;;
	(DONE)  ;;
	(HELP)
	    case "$arg0" in
		(HELO) resp="HELO <ver> <src>" ;;
		(IMPT) resp="IMPT <module>" ;;
		(EXPT) resp="EXPT <module>" ;;
		(DONE) resp="DONE <module>" ;;
		(*) resp="201 HELO, IMPT, EXPT, DONE" ;;
	    esac
	    ;;
	(*)
	    echo "Unknown command '$cmd'" >&2
	    resp="501 Bad Request"
	    ;;
    esac
    if test "$resp" ; then
	$verbose && echo "$progname> $resp" >&2
	echo "$resp"
    fi
done
