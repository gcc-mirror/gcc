[= autogen template sh=check.sh =]
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

export TARGET_MACHINE DESTDIR SRCDIR FIND_BASE VERBOSE INPUT

rm -rf ${DESTDIR} ${SRCDIR}
mkdir ${DESTDIR} ${SRCDIR}

( cd ${SRCDIR}
  set +e
  for f in [=

  _EVAL fix.files _stack _join "echo `

  for f in %s
  do case $f in
     */* ) echo $f | sed 's;/[^/]*$;;' ;;
     esac
  done | sort -u

  ` " _printf _shell =]
  do
    mkdir $f || mkdir -p $f
  done ) > /dev/null 2>&1

cd inc
[=

_FOR fix =][=

  _IF test_text _count 1 > =]
#
#  [=hackname=] has [=_EVAL test_text _count=] tests
#
sfile=[=
    _IF files _exist =][=
      files[0] =][=
    _ELSE =]testing.h[=
    _ENDIF =][=
    _FOR test_text FROM 1 =]
dfile=`dirname $sfile`/[=hackname "#_A-Z" "#-a-z" _tr=]-[=_EVAL _index=].h
cat >> $sfile <<_HACK_EOF_


#if defined( [=hackname _up=]_CHECK_[=_EVAL _index=] )
[=test_text=]
#endif  /* [=hackname _up=]_CHECK_[=_EVAL _index=] */
_HACK_EOF_
echo $sfile | ../../fixincl
mv -f $sfile $dfile
[ -f ${DESTDIR}/$sfile ] && mv ${DESTDIR}/$sfile ${DESTDIR}/$dfile[=
    /test_text =][=
  _ENDIF =][=

/fix

=][=

_FOR fix =][=

  _IF test_text _exist ! =][=
    _IF replace _exist ! =]
echo No test for [=hackname=] in inc/[=
      _IF files _exist =][=
        files[0] =][=
      _ELSE =]testing.h[=
      _ENDIF =][=
    _ENDIF =][=
  _ELSE =]
cat >> [=
    _IF files _exist =][=
      files[0] =][=
    _ELSE =]testing.h[=
    _ENDIF =] <<_HACK_EOF_


#if defined( [=hackname _up=]_CHECK )
[=test_text=]
#endif  /* [=hackname _up=]_CHECK */
_HACK_EOF_
[=_ENDIF =][=

/fix

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

_eval _outfile "chmod +x %s" _printf _shell

=]
