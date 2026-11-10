#!/bin/sh
# Copyright (C) 2026 Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# Regenerate the expected output for compiling core 1.49
#
# usage: regen-core-expected.sh BUILD_DIR
#
# BUILD_DIR is your build directory, the one with gcc/gccrs in it.  Uses
# the same flags and cleanup as core-diagnostics.exp.

if [ $# -ne 1 ] || [ ! -x "$1/gcc/gccrs" ]; then
  printf 'usage: %s BUILD_DIR\n' "$0" >&2
  printf 'BUILD_DIR should point to your GCC build directory\n' >&2
  exit 1
fi

bindir=$(cd "$1/gcc" && pwd)
here=$(cd "$(dirname "$0")" && pwd)
libdir=$(cd "$here/../../../../libgrust/rustc-lib" && pwd)

flags="-frust-incomplete-and-experimental-compiler-do-not-use
  -fdiagnostics-plain-output -fno-rust-debug -frust-edition=2018
  -frust-crate=core -w"

# the testsuite runs in the C locale, so quotes come out as plain '
set -x
LC_ALL=C "$bindir/gccrs" -B "$bindir" $flags -S "$libdir/core/src/lib.rs" \
  -o /dev/null 2>&1 \
  | sed -e "s|$libdir/||g" -e '/internal compiler error: /q' \
  > "$here/core-1.49.expected"
