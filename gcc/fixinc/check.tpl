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

export TARGET_MACHINE DESTDIR SRCDIR FIND_BASE

mkdir ${DESTDIR} ${SRCDIR}

( cd ${SRCDIR}
  mkdir sys X11 Xm sundev sunwindow )

[=

_FOR fix =][=

  _IF test_text _exist ! =][=
    _IF replace _exist ! =]
echo No test for [=hackname=][=
    _ENDIF =][=
  _ELSE =]
cat >> inc/[=
    _IF files _exist =][=
      files[0] =][=
    _ELSE =]testing.h[=
    _ENDIF =] <<- '_HACK_EOF_'


	#ifndef [=hackname _up=]_CHECK
	#define [=hackname _up=]_CHECK
[=test_text "\t" _prefix=]
	#endif  /* [=hackname _up=]_CHECK */
_HACK_EOF_
[=_ENDIF =][=

/fix

=]

cd inc
find . -type f | ../../fixincl
cd ..
diff -cr inc res | \
  sed -e 's;^\(\*\*\* inc/[^	]*\)	.*$;\1;' \
      -e 's;^\(--- res/[^	]*\)	.*$;\1;' > NEWDIFF
echo
echo Test output check:
[=

_FOR fix =][=

  _IF test_text _exist =]
fgrep [=hackname _up=]_CHECK NEWDIFF > /dev/null 2>&1 || \
  echo "[=_eval hackname _get "#%32s test failed.  See inc/"
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
