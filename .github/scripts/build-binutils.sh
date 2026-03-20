#!/bin/bash
# Build binutils for BPF target from a stable release.
#
# Usage: build-binutils.sh <install-dir> [binutils-version]
#
# binutils-version defaults to BINUTILS_VERSION env var, or 2.44.
set -euo pipefail

INSTALLDIR=$(realpath "$1")
BINUTILS_VERSION="${2:-${BINUTILS_VERSION:-2.44}}"
BINUTILS_TAG="binutils-${BINUTILS_VERSION//./_}"

if [ ! -d binutils-gdb ]; then
  git clone --depth 1 --branch "${BINUTILS_TAG}" \
    https://sourceware.org/git/binutils-gdb.git
fi

mkdir -p binutils-gdb/build-bpf
cd binutils-gdb/build-bpf
../configure --target=bpf-unknown-none --prefix="$INSTALLDIR" \
  --disable-nls --disable-werror \
  --disable-gdb --disable-gdbserver --disable-sim \
  --disable-libdecnumber --disable-readline \
  CFLAGS="-g -O2 -std=gnu11"
make -j"$(nproc)"
make install
