#! /bin/sh
#
#   fixinc.sco  --  Install modified versions of SCO system include
#   files.
#
#   Based on fixinc.svr4 script by Ron Guilmette (rfg@ncd.com) (SCO
#   modifications by Ian Lance Taylor (ian@airs.com)).
#
# This file is part of GNU CC.
# 
# GNU CC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# GNU CC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GNU CC; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
#
#	This script munges the native include files provided with SCO
#	3.2v4 systems so as to provide a reasonable namespace when
#	compiling with gcc.  The header files by default do not
#	provide many essential definitions and declarations if
#	__STDC__ is 1.  This script modifies the header files to check
#	for __STRICT_ANSI__ being defined instead.  Once munged, the
#	resulting new system include files are placed in a directory
#	that GNU C will search *before* searching the /usr/include
#	directory.  This script should work properly for most SCO
#	3.2v4 systems.  For other types of systems, you should use the
#	`fixincludes' or the `fixinc.svr4' script instead.
#
#	See README-fixinc for more information.

# Directory containing the original header files.
INPUT=${2-${INPUT-/usr/include}}

# Fail if no arg to specify a directory for the output.
if [ x$1 = x ]
then echo fixincludes: no output directory specified
exit 1
fi

# Directory in which to store the results.
LIB=${1?"fixincludes: output directory not specified"}

# Make sure it exists.
if [ ! -d $LIB ]; then
  mkdir $LIB || exit 1
fi

ORIG_DIR=`pwd`

# Make LIB absolute if it is relative.
# Don't do this if not necessary, since may screw up automounters.
case $LIB in
/*)
	;;
*)
	cd $LIB; LIB=`${PWDCMD-pwd}`
	;;
esac

echo 'Building fixincludes in ' ${LIB}

# Determine whether this filesystem has symbolic links.
if ln -s X $LIB/ShouldNotExist 2>/dev/null; then
  rm -f $LIB/ShouldNotExist
  LINKS=true
else
  LINKS=false
fi

echo 'Making directories:'
cd ${INPUT}
if $LINKS; then
  files=`ls -LR | sed -n s/:$//p`
else
  files=`find . -type d -print | sed '/^.$/d'`
fi
for file in $files; do
  rm -rf $LIB/$file
  if [ ! -d $LIB/$file ]
  then mkdir $LIB/$file
  fi
done

# treetops gets an alternating list
# of old directories to copy
# and the new directories to copy to.
treetops="${INPUT} ${LIB}"

if $LINKS; then
  echo 'Making internal symbolic directory links'
  for file in $files; do
    dest=`ls -ld $file | sed -n 's/.*-> //p'`
    if [ "$dest" ]; then    
      cwd=`pwd`
      # In case $dest is relative, get to $file's dir first.
      cd ${INPUT}
      cd `echo ./$file | sed -n 's&[^/]*$&&p'`
      # Check that the target directory exists.
      # Redirections changed to avoid bug in sh on Ultrix.
      (cd $dest) > /dev/null 2>&1
      if [ $? = 0 ]; then
	cd $dest
	# X gets the dir that the link actually leads to.
	x=`pwd`
	# If link leads back into ${INPUT},
	# make a similar link here.
	if expr $x : "${INPUT}/.*" > /dev/null; then
	  # Y gets the actual target dir name, relative to ${INPUT}.
	  y=`echo $x | sed -n "s&${INPUT}/&&p"`
	  echo $file '->' $y ': Making link'
	  rm -fr ${LIB}/$file > /dev/null 2>&1
	  ln -s ${LIB}/$y ${LIB}/$file > /dev/null 2>&1
	else
	  # If the link is to outside ${INPUT},
	  # treat this directory as if it actually contained the files.
# This line used to have $dest instead of $x.
# $dest seemed to be wrong for links found in subdirectories
# of ${INPUT}.  Does this change break anything?
	  treetops="$treetops $x ${LIB}/$file"
	fi
      fi
      cd $cwd
    fi
  done
fi

set - $treetops
while [ $# != 0 ]; do
  # $1 is an old directory to copy, and $2 is the new directory to copy to.
  echo "Finding header files in $1:"
  cd ${INPUT}
  cd $1
  files=`find . -name '*.h' -type f -print`
  echo 'Checking header files:'
  for file in $files; do
    if egrep '!__STDC__' $file >/dev/null; then
      if [ -r $file ]; then
	cp $file $2/$file >/dev/null 2>&1 || echo "Can't copy $file"
	chmod +w $2/$file
	chmod a+r $2/$file

# The following have been removed from the sed command below
# because it is more useful to leave these things in.
# The only reason to remove them was for -pedantic,
# which isn't much of a reason. -- rms.
#	  /^[ 	]*#[ 	]*ident/d

	sed -e '
	  s/!__STDC__/!defined (__STRICT_ANSI__)/g
	' $2/$file > $2/$file.sed
	mv $2/$file.sed $2/$file
	if cmp $file $2/$file >/dev/null 2>&1; then
	   rm $2/$file
	else
	   echo Fixed $file
	fi
      fi
    fi
  done
  shift; shift
done

# Fix first broken decl of getcwd present on some svr4 systems.

file=stdlib.h
base=`basename $file`
if [ -r ${LIB}/$file ]; then
  file_to_fix=${LIB}/$file
else
  if [ -r ${INPUT}/$file ]; then
    file_to_fix=${INPUT}/$file
  else
    file_to_fix=""
  fi
fi
if [ \! -z "$file_to_fix" ]; then
  echo Checking $file_to_fix
  sed -e 's/getcwd(char \{0,\}\*, int)/getcwd(char *, size_t)/' $file_to_fix > /tmp/$base
  if cmp $file_to_fix /tmp/$base >/dev/null 2>&1; then \
    true
  else
    echo Fixed $file_to_fix
    rm -f ${LIB}/$file
    cp /tmp/$base ${LIB}/$file
    chmod a+r ${LIB}/$file
  fi
  rm -f /tmp/$base
fi

# Fix second broken decl of getcwd present on some svr4 systems.  Also
# fix the incorrect decl of profil present on some svr4 systems.

file=unistd.h
base=`basename $file`
if [ -r ${LIB}/$file ]; then
  file_to_fix=${LIB}/$file
else
  if [ -r ${INPUT}/$file ]; then
    file_to_fix=${INPUT}/$file
  else
    file_to_fix=""
  fi
fi
if [ \! -z "$file_to_fix" ]; then
  echo Checking $file_to_fix
  sed -e 's/getcwd(char \*, int)/getcwd(char *, size_t)/' $file_to_fix \
    | sed -e 's/profil(unsigned short \*, unsigned int, unsigned int, unsigned int)/profil(unsigned short *, size_t, int, unsigned)/' > /tmp/$base
  if cmp $file_to_fix /tmp/$base >/dev/null 2>&1; then \
    true
  else
    echo Fixed $file_to_fix
    rm -f ${LIB}/$file
    cp /tmp/$base ${LIB}/$file
    chmod a+r ${LIB}/$file
  fi
  rm -f /tmp/$base
fi

# Fix third broken decl of getcwd on SCO.  Also fix incorrect decl of
# link.
file=prototypes.h
base=`basename $file`
if [ -r ${LIB}/$file ]; then
  file_to_fix=${LIB}/$file
else
  if [ -r ${INPUT}/$file ]; then
    file_to_fix=${INPUT}/$file
  else
    file_to_fix=""
  fi
fi
if [ \! -z "$file_to_fix" ]; then
  echo Checking $file_to_fix
  sed -e 's/getcwd(char \*, int)/getcwd(char *, size_t)/' $file_to_fix \
    | sed -e 's/const  int	link(const char \*, char \*)/extern  int	link(const char *, const char *)/' > /tmp/$base
  if cmp $file_to_fix /tmp/$base >/dev/null 2>&1; then \
    true
  else
    echo Fixed $file_to_fix
    rm -f ${LIB}/$file
    cp /tmp/$base ${LIB}/$file
    chmod a+r ${LIB}/$file
  fi
  rm -f /tmp/$base
fi

# Fix an error in this file: the #if says _cplusplus, not the double
# underscore __cplusplus that it should be
file=tinfo.h
if [ -r $file ] && [ ! -r ${LIB}/$file ]; then
  mkdir ${LIB}/rpcsvc 2>/dev/null
  cp $file ${LIB}/$file >/dev/null 2>&1 || echo "Can't copy $file"
  chmod +w ${LIB}/$file 2>/dev/null
  chmod a+r ${LIB}/$file 2>/dev/null
fi

if [ -r ${LIB}/$file ]; then
  echo Fixing $file, __cplusplus macro
  sed -e 's/[ 	]_cplusplus/ __cplusplus/' ${LIB}/$file > ${LIB}/${file}.sed
  rm -f ${LIB}/$file; mv ${LIB}/${file}.sed ${LIB}/$file
  if cmp $file ${LIB}/$file >/dev/null 2>&1; then
    rm ${LIB}/$file
  fi
fi

# Fix prototype declaration of utime in sys/times.h.  In 3.2v4.0 the
# const is missing.
file=sys/times.h
if [ -r $file ] && [ ! -r ${LIB}/$file ]; then
  cp $file ${LIB}/$file >/dev/null 2>&1 || echo "Can't copy $file"
  chmod +w ${LIB}/$file 2>/dev/null
  chmod a+r ${LIB}/$file 2>/dev/null
fi

if [ -r ${LIB}/$file ]; then
  echo Fixing $file, utime prototype
  sed -e 's/(const char \*, struct utimbuf \*);/(const char *, const struct utimbuf *);/' ${LIB}/$file > ${LIB}/${file}.sed
  rm -f ${LIB}/$file; mv ${LIB}/${file}.sed ${LIB}/$file
  if cmp $file ${LIB}/$file >/dev/null 2>&1; then
    rm ${LIB}/$file
  fi
fi

echo 'Removing unneeded directories:'
cd $LIB
files=`find . -type d -print | sort -r`
for file in $files; do
  rmdir $LIB/$file > /dev/null 2>&1
done

if $LINKS; then
  echo 'Making internal symbolic non-directory links'
  cd ${INPUT}
  files=`find . -type l -print`
  for file in $files; do
    dest=`ls -ld $file | sed -n 's/.*-> //p'`
    if expr "$dest" : '[^/].*' > /dev/null; then    
      target=${LIB}/`echo file | sed "s|[^/]*\$|$dest|"`
      if [ -f $target ]; then
        ln -s $dest ${LIB}/$file >/dev/null 2>&1
      fi
    fi
  done
fi

exit 0
