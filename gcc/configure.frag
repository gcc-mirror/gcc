# configure.frag for GNU CC
# Process the Makefile fragments from language directories.

# Copyright (C) 1997 Free Software Foundation, Inc.

#This file is part of GNU CC.

#GNU CC is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 2, or (at your option)
#any later version.

#GNU CC is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with GNU CC; see the file COPYING.  If not, write to
#the Free Software Foundation, 59 Temple Place - Suite 330,
#Boston, MA 02111-1307, USA.

# First parameter is the source directory, second is list of subdirectories

savesrcdir=$1
savesubdirs=$2

# First ensure the language build subdirectories exist.

for subdir in . $savesubdirs
do
	if [ $subdir != . ]
	then
		test -d $subdir || mkdir $subdir
	fi
done

# Now copy each language's Make-lang.in file to Make-lang.

rm -f Make-lang
touch Make-lang

for subdir in . $savesubdirs
do
	if [ $subdir != . ]
	then
		cat $savesrcdir/$subdir/Make-lang.in >> Make-lang
	fi
done
