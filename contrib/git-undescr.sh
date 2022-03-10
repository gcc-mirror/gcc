#!/bin/sh

# Script to undescribe a GCC revision

o=$(git config --get gcc-config.upstream);
r=$(echo $1 | sed -n 's,^r\([0-9][0-9]*\)-[0-9][0-9]*\(-g[0-9a-f]*\)*$,\1,p');
n=$(echo $1 | sed -n 's,^r[0-9][0-9]*-\([0-9][0-9]*\)\(-g[0-9a-f]*\)*$,\1,p');

test -z $r && echo Invalid id $1 && exit 1;
h=$(git rev-parse --verify --quiet ${o:-origin}/releases/gcc-$r);
test -z $h && h=$(git rev-parse --verify --quiet ${o:-origin}/master);
p=$(git describe --all --match 'basepoints/gcc-'$r $h | sed -n 's,^tags/,,;s,^basepoints/gcc-[0-9][0-9]*-\([0-9][0-9]*\)-g[0-9a-f]*$,\1,p;s,^basepoints/gcc-[0-9][0-9]*$,0,p');
git rev-parse --verify $h~$(expr $p - $n);
