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

rm -f Make-lang
touch Make-lang

# We can either be invoked with . from configure or from Makefile.
# Some shells can't pass arguments to source'd scripts.
# ??? This needs some rethinking.

savesrcdir=$srcdir
savesubdirs=$subdirs

for subdir in . $savesubdirs
do
	oldsrcdir=$savesrcdir

	# Re-adjust the path
	case $oldsrcdir in
	/*)
		case $subdir in
		.)
			srcdir=$oldsrcdir
			;;
		*)
			srcdir=$oldsrcdir/$subdir
			;;
		esac
		;;
	*)
		case $subdir in
		.)
			;;
		*)
			oldsrcdir=../${oldsrcdir}
			srcdir=$oldsrcdir/$subdir
			;;
		esac
		;;
	esac
	mainsrcdir=$oldsrcdir
	STARTDIR=`pwd`
	test -d $subdir || mkdir $subdir
	cd $subdir

	# If this is the top level Makefile, add the language fragments.
	if [ $subdir = . ]
	then
		for s in .. $savesubdirs
		do
			if [ $s != ".." ]
			then
				cat ${mainsrcdir}/$s/Make-lang.in >> Make-lang
			fi
		done
	fi

	cd $STARTDIR
done   # end of current-dir SUBDIRS loop

srcdir=$savesrcdir
