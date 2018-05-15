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

VERSION=0
progname=${0##*/}
main=
verbose=false
shopt -s extglob nullglob

compile () {
    local module=$1
    local ign=false
    local cmd="$COLLECT_GCC"

    if test -z "$cmd" ; then
	echo "not inferior of compiler driver"
	return
    fi
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
	echo "cannot find module source"
	return
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

while read -a args -p "moracle>" ; do
    $verbose && echo "$progname< ${args[@]}" >&2
    resp=
    case "${args[0]}" in
	(HELLO)
	    if test "${args[1]}" = $VERSION ; then
		main="${args[2]}"
		resp="HELLO $VERSION"
	    else
		resp="ERROR Bad Version (expect $VERSION)"
	    fi
	    ;;
	(BMI)
	    file=$(echo ${args[1]} | tr . -)
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
	    file=$(echo ${args[1]} | tr . -)
	    resp="BMI $file.nms"
	    ;;
	(DONE)
	    resp="OK"
	    ;;
	(HELP)
	    case "${args[1]}" in
		(HELLO) resp="HELLO <ver> <src>" ;;
		(BMI) resp="BMI <module> [<from>]" ;;
		(EXPORT) resp="EXPORT <module>" ;;
		(DONE) resp="DONE <module>" ;;
		(*) resp="HELP HELLO, BMI, EXPORT, DONE" ;;
	    esac
	    ;;
	(*)
	    echo "Unknown command '${args[0]}'" >&2
	    resp="ERROR Bad Request"
	    ;;
    esac
    if test "$resp" ; then
	$verbose && echo "$progname> $resp" >&2
	echo "$resp"
    fi
done
