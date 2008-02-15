#! /bin/sh
#
# Check that the warning flags documented in invoke.texi match up
# with what the compiler accepts.
#
# Copyright (C) 2008 Free Software Foundation, Inc.
# Written by Ralf Wildenhues <Ralf.Wildenhues@gmx.de>.
#
# This script is Free Software, and it can be copied, distributed and
# modified as defined in the GNU General Public License.  A copy of
# its license can be downloaded from http://www.gnu.org/copyleft/gpl.html
#
# Call this script as
#    check_warning_flags.sh path/to/invoke.texi
# with CC set to the compiler to be tested.
# The script scribbles in the current directory.

progname=`echo "$0" | sed 's,.*/,,'`
usage ()
{
  echo "usage: $progname path/to/invoke.texi"
  echo "set \$CC to the compiler to be checked"
  exit 1
}

ret=0
LC_ALL=C
export LC_ALL
: ${CC=gcc}
test $# = 1 || usage
invoke_texi=$1
test -r "$invoke_texi" || {
  echo "$progname: error: cannot read '$invoke_texi'" >&2
  usage
}
filebase=check_warning_flags_file$$
stderr=check_warning_flags_stderr$$

remove_problematic_flags='
  /-Wlarger-than-/d
  /-W[alp],/d
  /-Werror/d
  /-Wpadded/d
  /=/d'

# Ensure that indexed warnings are accepted.
set x `sed '/^@opindex W/{
  s/^@opindex /-/
  '"$remove_problematic_flags"'
  /-W[alp]$/d
  p
}
d' <"$invoke_texi"`
shift
: >$filebase.c
$CC -c $filebase.c "$@" 2>&1 |
  grep -v 'command line option.*is valid for.*but not for' >$stderr
if test -s $stderr; then
  echo "options listed in @opindex but not accepted by the compiler:" >&2
  cat $stderr >&2
  ret=1
fi
rm -f $filebase.c $stderr

# Check documentation of warning options.
for lang in c c++ objc obj-c++; do
  case $lang in
  c)       ext=c; langmatch='[^-]C[^+].*only' ;;
  c++)     ext=C; langmatch='[^-]C++.*only' ;;
  objc)    ext=m; langmatch='Objective-C[^+].*only' ;;
  obj-c++) ext=M; langmatch='Objective-C++.*only' ;;
  esac
  file=$filebase.$ext
  : >$file
  $CC -c $file 2>$stderr
  if grep 'not installed on this system' $stderr >/dev/null ||
    grep 'installation problem, cannot exec' $stderr >/dev/null ||
    grep 'error trying to exec' $stderr >/dev/null
  then
    echo "$progname: $CC is not configured for language $lang, skipping checks" >&2
    rm -f $file $filebase.o $filebase.obj $stderr
    continue
  fi

  # Verify good warning flags.
  set x `sed '
    t a
    :a
    /^@item -W/{
      /'"$langmatch"'/b x
      / only)/d
      b x
    }
    d
    :x
    '"$remove_problematic_flags"'
    s/^@item //
    s/ .*//
    ' <"$invoke_texi"`
  shift
  $CC -c $file -O "$@" 2>$stderr
  if test -s $stderr; then
    echo failures:  >&2
    cat $stderr >&2
    ret=1
  fi

  # Verify bad warning flags.
  set x `sed '
    t a
    :a
    /^@item -W/{
      / only)/!d
      /'"$langmatch"'/d
      b x
    }
    d
    :x
    '"$remove_problematic_flags"'
    s/^@item //
    s/ .*//
    ' <"$invoke_texi"`
  shift
  $CC -c $file -O "$@" 2>$stderr
  # cat $stderr >&2
  test $# = `grep 'command line option.*valid.*but not for' <$stderr | wc -l` || {
    for warning
    do
      grep "command line option.*$warning.*valid" <$stderr >&2 ||
	echo "valid for $lang but not annotated as such: $warning"
    done
    ret=1
  }
  rm -f $file $filebase.o $filebase.obj $stderr
done
exit $ret
