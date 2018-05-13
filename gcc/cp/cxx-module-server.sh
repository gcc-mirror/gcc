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

VERSION=@VERSION@
test "$VERSION" != '@'VERSION'@' || VERSION=0
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
    $cmd || echo "compilation $src failed"
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
	(HELLO)
	    if test $arg0 = $VERSION ; then
		main="$arg1"
		resp="HELLO $VERSION"
	    else
		resp="ERROR Bad Version (expect $VERSION)"
	    fi
	    ;;
	(BMI)
	    file=$(echo $arg0 | tr . -)
	    comp=
	    if ! test -e $file.nms ; then
		resp=$(compile $file)
	    fi
	    if test -e $file.nms ; then
		resp="BMI $file.nms"
	    else
		resp="ERROR $resp"
	    fi
	    ;;
	(EXPORT)
	    file=$(echo $arg0 | tr . -)
	    resp="BMI $file.nms"
	    ;;
	(DONE)
	    resp="OK"
	    ;;
	(HELP)
	    case "$arg0" in
		(HELLO) resp="HELLO <ver> <src>" ;;
		(BMI) resp="BMI <module>" ;;
		(EXPORT) resp="EXPORT <module>" ;;
		(DONE) resp="DONE <module>" ;;
		(*) resp="HELP HELLO, BMI, EXPORT, DONE" ;;
	    esac
	    ;;
	(*)
	    echo "Unknown command '$cmd'" >&2
	    resp="ERROR Bad Request"
	    ;;
    esac
    if test "$resp" ; then
	$verbose && echo "$progname> $resp" >&2
	echo "$resp"
    fi
done
