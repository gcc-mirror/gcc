[= autogen template sh=check.sh =]
[=
#
#  This file contanes the shell template to run tests on the fixes
#
=]#!/bin/sh

set -e
[ -d testdir ] && rm -rf testdir
mkdir testdir
cd testdir

TARGET_MACHINE='*'
DESTDIR=`pwd`/res
SRCDIR=`pwd`/inc
FIND_BASE='.'
VERBOSE=1

export TARGET_MACHINE DESTDIR SRCDIR FIND_BASE VERBOSE

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

[=

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
cat >> inc/[=
    _IF files _exist =][=
      files[0] =][=
    _ELSE =]testing.h[=
    _ENDIF =] <<- _HACK_EOF_


	#if defined( [=hackname _up=]_CHECK )
[=test_text "\t" _prefix=]
	#endif  /* [=hackname _up=]_CHECK */
_HACK_EOF_
[=_ENDIF =][=

/fix

=]

cd inc
find . -type f | sed 's;\./;;' | sort > ../LIST
../../fixincl < ../LIST
cd ..

while read f
do
  if [ ! -f res/$f ]
  then
    echo "Only in inc:  inc/$f"
  else
    diff -c inc/$f res/$f | \
      sed -e '1,2s;	.*;;' -e '/MACH_DIFF:/,/no uniform test,/d'
  fi
done > NEWDIFF < LIST

echo
echo Test output check:
[=

_FOR fix =][=

  _IF test_text _exist =]
fgrep [=hackname _up=]_CHECK NEWDIFF > /dev/null 2>&1 || \
  echo "[=_eval hackname _get "#%32s test failed.  See testdir/inc/"
          _printf =][=
    _IF files _exist =][=
      files[0] =][=
    _ELSE =]testing.h[=
    _ENDIF =]"[=
  _ENDIF =][=

/fix

=][=
_eval _outfile "chmod +x %s" _printf _shell
=]
