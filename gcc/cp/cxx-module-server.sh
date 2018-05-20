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

cmd () {
    resp=
    case "$1" in
	(ASYNC)
    	    # We respond to async requests immediately
	    shift
	    cmd "$@"
	    ;;
	(BMI)
	    # We try and build a BMI from source
	    file=$(echo $2 | tr . -)
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
	    file=$(echo $2 | tr . -)
	    resp="BMI $file.nms"
	    ;;
	(DONE)
	    resp="OK"
	    ;;
	(PEEK)
	    # we don't do anything with a peek
	    resp=OK
	    ;;
	(HELLO)
	    if test "$2" = $VERSION ; then
		main="$3"
		resp="HELLO $VERSION"
	    else
		resp="ERROR Bad version (expect $VERSION)"
	    fi
	    ;;
	(HELP)
	    case "$2" in
		(query) resp="BMI|SEARCH|PATH" ;;
		(ASYNC) resp="ASYNC query" ;;
		(BMI) resp="BMI <module> [<from>]" ;;
		(DONE) resp="DONE <module>" ;;
		(EXPORT) resp="EXPORT <module>" ;;
		(FUTURE) resp="FUTURE query" ;;
		(HELLO) resp="HELLO <ver> <src>" ;;
		(PATH) resp="PATH <module> [<from>]" ;;
		(SEARCH) resp="SEARCH <module> [<from>]" ;;
		(*) resp="HELP HELLO, BMI, EXPORT, DONE" ;;
	    esac
	    ;;
	(PATH)
	    resp="ERROR Unimplemented"
	    ;;
	(RESET)
	    main=""
	    ;;
	(SEARCH)
	    resp="$(echo $2 | tr . -).cc"
	    ;;
	(*)
	    echo "Unknown command '$1'" >&2
	    resp="ERROR Bad Request"
	    ;;
    esac
    if test "$resp" ; then
	$verbose && echo "$progname> $resp" >&2
	echo "$resp"
    fi
}

while test "$#" != 0 ; do
    case "$1" in
	(-v) verbose=true ;;
	(-*) echo "Unknown option '$1'" >&2 ; exit 1 ;;
	(*) break ;;
    esac
    shift
done


# module oracle -> modacle
while read -a args -p "modacle>" ; do
    $verbose && echo "$progname< ${args[@]}" >&2
    cmd "${args[@]}"
done
