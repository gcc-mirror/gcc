#! /bin/sh
#
# Check for accurate dependencies in gcc/Makefile.in.
#
# Copyright (C) 2008 Free Software Foundation, Inc.
# Written by Ralf Wildenhues <Ralf.Wildenhues@gmx.de>.
#
# This script is Free Software, and it can be copied, distributed and
# modified as defined in the GNU General Public License.  A copy of
# its license can be downloaded from http://www.gnu.org/copyleft/gpl.html
#
# Start this script in an up to date build-tree/gcc directory.
# Using it in stage1 only works if the host compiler is GCC.

# To continue an interrupted check, make sure there are no *.o.backup
# files lying around (i.e., move them back to their original name),
# and set $start_after to the name of the last object that should be skipped.
start_after=

# Skip some objects unconditionally; make sure each name in this list is
# surrounded by spaces.
skip=" crtbegin.o crtbeginS.o crtbeginT.o crtend.o crtendS.o crtfastmath.o crtprec64.o crtprec80.o crtprec32.o ecrti.o ecrtn.o ncrti.o ncrtn.o "

# Files which show up as dependencies other than through unconditional #include.
# This is an egrep pattern.
hidden_dep_files='(BASE-VER|DATESTAMP|DEV-PHASE|Makefile|xcoffout\.h|basic-block\.h|bconfig\.h)$'

: ${MAKE=make}
: ${EGREP="grep -E"}

# -------------------------------------------------------------------------
# There should be no need for changes beyond this point.

set -e
st=0

if test -f c-family/c-common.o; then :; else
  echo "$0: rerun in an up to date build-tree/gcc directory" >&2
  exit 1
fi

for obj in *.o
do
  if test -n "$start_after"; then
    if test $obj = $start_after; then
      start_after=
    fi
    continue
  fi
  case $skip in *\ $obj\ *) continue ;; esac

  mv -f $obj $obj.backup
  ${MAKE} $obj CFLAGS='-MM -MF depfile'
  mv -f $obj.backup $obj
  ${MAKE} -t
  LC_ALL=C ${MAKE} -d $obj >make-d-log
  hdrs=`cat depfile`
  for hdr in $hdrs; do
    case $hdr in
      *: | *.o | \\ | /* ) ;;
      *)
        echo $hdr ;;
    esac
  done < depfile |
  LC_ALL=C sort -u > hdrs


  sed -n '/.*Prerequisite..\([^ ]*\). is newer than target .'"$obj"'.*/s//\1/p' \
    < make-d-log |
  LC_ALL=C sort -u > not-up-to-date
  if test -s not-up-to-date; then
    st=1
    echo "$0: error: prerequisites for $obj are not up to date:" >&2
    cat not-up-to-date >&2
  fi
  sed -n '/.*Prerequisite..\([^ ]*\). is older than target .'"$obj"'.*/s//\1/p' \
    < make-d-log |
  LC_ALL=C sort -u > deps
  missing_deps=`LC_ALL=C join -v 1 hdrs deps`
  unneeded_deps=`LC_ALL=C join -v 2 hdrs deps | $EGREP -v "$hidden_dep_files" || :`
  if test -n "$missing_deps"; then
    st=1
    echo "missing deps for $obj:"
    echo "$missing_deps" | sed 's/^/  /'
  fi
  if test -n "$unneeded_deps"; then
    # unneeded dependencies are not a big problem, so they cause no failure.
    echo "unneeded deps for $obj:"
    echo "$unneeded_deps" | sed 's/^/  /'
  fi
done
exit $st

# vi:sw=2:
