[= autogen5 template sh=check.sh =]
[=
#
#  This file contanes the shell template to run tests on the fixes
#
=]#!/bin/sh

set -e
TESTDIR=tests
TESTBASE=`cd $1;pwd`

[ -d ${TESTDIR} ] || mkdir ${TESTDIR}
cd ${TESTDIR}
TESTDIR=`pwd`

TARGET_MACHINE='*'
DESTDIR=`pwd`/res
SRCDIR=`pwd`/inc
FIND_BASE='.'
VERBOSE=1
INPUT=`pwd`
ORIGDIR=${INPUT}

export TARGET_MACHINE DESTDIR SRCDIR FIND_BASE VERBOSE INPUT ORIGDIR

rm -rf ${DESTDIR} ${SRCDIR}
mkdir ${DESTDIR} ${SRCDIR}

( cd ${SRCDIR}
  set +e
  for f in [=

  (shellf "echo `

  for f in %s
  do case $f in
     */* ) echo $f | sed 's;/[^/]*$;;' ;;
     esac
  done | sort -u

  ` " (join " " (stack "fix.files"))  ) =]
  do
    mkdir $f || mkdir -p $f
  done ) > /dev/null 2>&1

cd inc
[=

FOR fix =][=

  IF (> (count "test_text") 1) =]
#
#  [=hackname=] has [=(count "test_text")=] tests
#
sfile=[=
    IF (exist? "files") =][=
      files[] =][=
    ELSE  =]testing.h[=
    ENDIF =][=
    FOR test_text FROM 1 =]
dfile=`dirname $sfile`/[=(string-tr! (get "hackname") "_A-Z" "-a-z")
                        =]-[=(for-index)=].h
cat >> $sfile <<_HACK_EOF_


#if defined( [=(string-upcase! (get "hackname"))=]_CHECK_[=(for-index)=] )
[=test_text=]
#endif  /* [=(string-upcase! (get "hackname"))=]_CHECK_[=(for-index)=] */
_HACK_EOF_
echo $sfile | ../../fixincl
mv -f $sfile $dfile
[ -f ${DESTDIR}/$sfile ] && mv ${DESTDIR}/$sfile ${DESTDIR}/$dfile[=
    ENDFOR  test_text =][=
  ENDIF               =][=

ENDFOR  fix

=][=

FOR fix =][=

  IF (not (exist? "test_text")) =][=
    IF (not (exist? "replace")) =]
echo No test for [=hackname=] in inc/[=
      IF (exist? "files")       =][=
        files[0] =][=
      ELSE  =]testing.h[=
      ENDIF =][=
    ENDIF   =][=
  ELSE      =]
cat >> [=
    IF (exist? "files") =][=
      files[0] =][=
    ELSE =]testing.h[=
    ENDIF =] <<_HACK_EOF_


#if defined( [=(string-upcase! (get "hackname"))=]_CHECK )
[=test_text=]
#endif  /* [=(string-upcase! (get "hackname"))=]_CHECK */
_HACK_EOF_
[=ENDIF =][=

ENDFOR  fix

=]

find . -type f | sed 's;\./;;' | sort | ../../fixincl
cd ${DESTDIR}

exitok=true

find * -type f -print > ${TESTDIR}/LIST

#  Special hack for sys/types.h:  the #define-d types for size_t,
#  ptrdiff_t and wchar_t are different for each port.  Therefore,
#  strip off the defined-to type so that the test results are the
#  same for all platforms.
#
sed 's/\(#define __[A-Z_]*_TYPE__\).*/\1/' sys/types.h > XX
mv -f XX sys/types.h

#  The following subshell weirdness is for saving an exit
#  status from within a while loop that reads input.  If you can
#  think of a cleaner way, suggest away, please...
#
exitok=`
exec < ${TESTDIR}/LIST
while read f
do
  if [ ! -f ${TESTBASE}/$f ]
  then
    echo "Newly fixed header:  $f" >&2
    exitok=false

  elif cmp $f ${TESTBASE}/$f >&2
  then
    :

  else
    diff -c $f ${TESTBASE}/$f >&2 || :
    exitok=false
  fi
done
echo $exitok`

cd $TESTBASE

find * -type f -print | \
fgrep -v 'CVS/' > ${TESTDIR}/LIST

exitok=`
exec < ${TESTDIR}/LIST
while read f
do
  if [ -s $f ] && [ ! -f ${DESTDIR}/$f ]
  then
    echo "Missing header fix:  $f" >&2
    exitok=false
  fi
done
echo $exitok`

echo
if $exitok
then
  cd ${TESTDIR}
  rm -rf inc res LIST
  cd ..
  rmdir ${TESTDIR} > /dev/null 2>&1 || :
  echo All fixinclude tests pass >&2
else
  echo There were fixinclude test FAILURES  >&2
fi
$exitok[=

(set-writable)

=]
