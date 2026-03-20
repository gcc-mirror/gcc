#!/bin/bash
# Build GCC for BPF target from the current repo source.
#
# Usage: build-gcc.sh <gcc-source-dir> <install-dir>
#
# Expects binutils for bpf-unknown-none to already be installed
# in <install-dir> (so GCC's configure finds the assembler/linker).
set -euo pipefail

SRCDIR=$(realpath "$1")
INSTALLDIR=$(realpath "$2")

cd "$SRCDIR"
for i in 1 2 3; do
  ./contrib/download_prerequisites --force && break
  echo "Retry $i: download_prerequisites failed, retrying..."
  sleep 5
done
cd -

BUILDDIR="${SRCDIR}/build-bpf"
rm -rf "$BUILDDIR"
mkdir -p "$BUILDDIR"
cd "$BUILDDIR"

"${SRCDIR}/configure" \
  --target=bpf-unknown-none \
  --prefix="$INSTALLDIR" \
  --disable-nls \
  --enable-languages=c \
  --without-headers \
  --disable-multilib

make -j"$(nproc)"
make install
