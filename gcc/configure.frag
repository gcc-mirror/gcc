# configure.frag for GCC
# Process the host/target/language Makefile fragments.

# Copyright (C) 1997 Free Software Foundation, Inc.

#This file is part of GCC.

#GCC is free software; you can redistribute it and/or modify it under
#the terms of the GNU General Public License as published by the Free
#Software Foundation; either version 2, or (at your option) any later
#version.

#GCC is distributed in the hope that it will be useful, but WITHOUT
#ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#for more details.

#You should have received a copy of the GNU General Public License
#along with GCC; see the file COPYING.  If not, write to the Free
#Software Foundation, 59 Temple Place - Suite 330, Boston, MA
#02111-1307, USA.

# First parameter is the source directory, second is list of subdirectories,
# third is list of host makefile fragments, fourth is list of target makefile
# fragments.

srcdir=$1
subdirs=$2
xmake_files=$3
tmake_files=$4

# Copy all the host makefile fragments into Make-host.

rm -f Make-host
touch Make-host
for f in .. $xmake_files
do
	if [ -f $f ]
	then
		cat $f >> Make-host
	fi
done

# Copy all the target makefile fragments into Make-target.

rm -f Make-target
touch Make-target
for f in .. $tmake_files
do
	if [ -f $f ]
	then
		cat $f >> Make-target
	fi
done

# Ensure the language build subdirectories exist.

for subdir in . $subdirs
do
	if [ $subdir != . ]
	then
		test -d $subdir || mkdir $subdir
	fi
done

# Now copy each language's Make-lang.in file to Make-lang.

rm -f Make-lang
touch Make-lang

for subdir in . $subdirs
do
	if [ $subdir != . ]
	then
		cat $srcdir/$subdir/Make-lang.in >> Make-lang
	fi
done
