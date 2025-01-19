#! /bin/sh
# Script to generate SYSROOT_SUFFIX_SPEC equivalent to MULTILIB_OSDIRNAMES
# Arguments are MULTILIB_OSDIRNAMES, MULTILIB_OPTIONS, MULTILIB_MATCHES,
# and MULTILIB_REUSE.

# Copyright (C) 2009-2025 Free Software Foundation, Inc.

# This file is part of GCC.

# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.

# GCC is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.  

# This shell script produces a header file fragment that defines
# SYSROOT_SUFFIX_SPEC.  It assumes that the sysroots will have the same
# structure and names used by the multilibs.

# Invocation:
#   print-sysroot-suffix.sh \
#          MULTILIB_OSDIRNAMES \
#          MULTILIB_OPTIONS \
#          MULTILIB_MATCHES \
#          MULTILIB_REUSE
#      > t-sysroot-suffix.h

# The four options exactly correspond to the variables of the same
# names defined in the t-sysroot-suffix tmake_file fragment.

# Example:
#   sh ./gcc/config/print-sysroot-suffix.sh "a=A" "a b/c/d" ""
# =>
#   #undef SYSROOT_SUFFIX_SPEC
#   #define SYSROOT_SUFFIX_SPEC "" \
#   "%{a:" \
#     "%{b:A/b/;" \
#     "c:A/c/;" \
#     "d:A/d/;" \
#     ":A/};" \
#   ":}"

# The script uses temporary subscripts in order to permit a recursive
# algorithm without the use of functions.

set -e

dirnames="$1"
options="$2"
matches="$3"
reuse="$4"

cat > print-sysroot-suffix3.sh <<\EOF
#! /bin/sh
# Print all the multilib matches for this option
result="$1"
EOF
for x in $matches; do
  l=`echo $x | sed -e 's/=.*$//' -e 's/?/=/g'`
  r=`echo $x | sed -e 's/^.*=//' -e 's/?/=/g'`
  echo "[ \"\$1\" = \"$l\" ] && result=\"\$result|$r\"" >> print-sysroot-suffix3.sh
done
echo 'echo $result' >> print-sysroot-suffix3.sh
chmod +x print-sysroot-suffix3.sh

cat > print-sysroot-suffix2.sh <<\EOF
#! /bin/sh
# Recursive script to enumerate all multilib combinations, match against
# multilib directories and output a spec string of the result.
# Will fold identical trees.

padding="$1"
optstring="$2"
shift 2
n="\" \\
$padding\""
if [ $# = 0 ]; then
  case $optstring in
EOF
for x in $reuse; do
  l=`echo $x | sed -e 's/=.*$//' -e 's/\./=/g'`
  r=`echo $x | sed -e 's/^.*=//' -e 's/\./=/g'`
  echo "/$r/) optstring=\"/$l/\" ;;" >> print-sysroot-suffix2.sh
done
echo "  esac" >> print-sysroot-suffix2.sh

pat=
for x in $dirnames; do
  p=`echo $x | sed -e 's,=!,/$=/,'`
  pat="$pat -e 's=^//$p='"
done
echo '  optstring=`echo "/$optstring" | sed '"$pat\`" >> print-sysroot-suffix2.sh
cat >> print-sysroot-suffix2.sh <<\EOF
  case $optstring in
  //*)
    ;;
  *)
    echo "$optstring"
    ;;
  esac
else
  thisopt="$1"
  shift
  bit=
  lastcond=
  result=
  for x in `echo "$thisopt" | sed -e 's,/, ,g'`; do
    case $x in
EOF
for x in `echo "$options" | sed -e 's,/, ,g'`; do
  match=`./print-sysroot-suffix3.sh "$x"`
  echo "$x) optmatch=\"$match\" ;;" >> print-sysroot-suffix2.sh
done
cat >> print-sysroot-suffix2.sh <<\EOF
    esac
    bit=`"$0" "$padding  " "$optstring$x/" "$@"`
    if [ -z "$lastopt" ]; then
      lastopt="$optmatch"
    else
      if [ "$lastbit" = "$bit" ]; then
	lastopt="$lastopt|$optmatch"
      else
	result="$result$lastopt:$lastbit;$n"
	lastopt="$optmatch"
      fi
    fi
    lastbit="$bit"
  done
  bit=`"$0" "$padding  " "$optstring" "$@"`
  if [ "$bit" = "$lastbit" ]; then
    if [ -z "$result" ]; then
      echo "$bit"
    else
      echo "$n%{$result:$bit}"
    fi
  else
    echo "$n%{$result$lastopt:$lastbit;$n:$bit}"
  fi
fi
EOF

chmod +x ./print-sysroot-suffix2.sh
result=`./print-sysroot-suffix2.sh "" "/" $options`
echo "#undef SYSROOT_SUFFIX_SPEC"
echo "#define SYSROOT_SUFFIX_SPEC \"$result\""
rm print-sysroot-suffix2.sh
rm print-sysroot-suffix3.sh
