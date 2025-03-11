#!/bin/sh

if [ "$1" = "--help" -o $# -ne 2 -o -f "$1" ]; then
    echo Usage: relpath.sh FROM TO
    echo Print the relative path from FROM to TO
    echo FROM must be a directory, but need not exist
    exit 0
fi

from="${1%%/}"
to="${2%%/}"

# The parent directory of a pathname, handling ..
parent() {
    name=$(basename "$1")
    path=$(dirname "$1")
    top=$(basename "$path")
    if [ "$top" = ".." ]; then
	path=$(parent "$path")
    fi
    if [ "$name" = ".." ]; then
	path=$(parent "$path")
    fi
    echo $path
}

# Canonicalize a directory that contains '..'.
canonicalize() {
    path=$1
    suffix=
    while ! [ -d "$path" ]; do
	name=$(basename "$path")
	path=$(parent "$path")
	suffix="/$name$suffix"
    done
    if [ -d "$path" ]; then
	echo $(cd "$path"; pwd)$suffix
    else
	echo $1
    fi
}

case "$to$from" in
    *..* )
	from=$(canonicalize "$from")
	to=$(canonicalize "$to")
	;;
esac
case "$to$from" in
    *..* )
	echo unable to canonicalize .. >&2
	exit 1
	;;
esac

back=
while [ "${to#$from}" = "$to" ]; do
    #echo $from too long
    from=$(dirname $from);
    back=../$back

    if [ "$from" = / ] && [ "${to#/}" = "$to" ]; then
	echo no common ancestor between $1 and $2 >&2
	exit 1
    elif [ "$from" = . ]; then
	echo no common ancestor between $1 and $2 >&2
	exit 1
    fi
done

to=${to#$from}
to=${to##/}
back=${back%%/}

if [ -n "$to" ] && [ -n "$back" ]; then
    echo $back/$to
elif [ -n "$back$to" ]; then
    echo $back$to
else
    echo .
fi
