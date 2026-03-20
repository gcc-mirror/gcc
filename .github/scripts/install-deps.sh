#!/bin/bash
# Install system dependencies required to build GCC and binutils from source.
# Supports: Ubuntu, Debian, Fedora, CentOS.
set -euo pipefail

if [ -f /etc/os-release ]; then
  . /etc/os-release
else
  echo "Cannot detect distro: /etc/os-release not found" >&2
  exit 1
fi

case "$ID" in
  ubuntu|debian)
    sudo apt-get -y update
    sudo apt-get -y install \
      git wget build-essential file gawk flex bison texinfo libgmp-dev libmpfr-dev libmpc-dev
    ;;
  fedora)
    sudo dnf -y install \
      git wget gcc gcc-c++ make diffutils file flex bison texinfo gmp-devel mpfr-devel libmpc-devel
    ;;
  centos|rhel)
    sudo dnf -y install \
      git wget gcc gcc-c++ make diffutils file flex bison texinfo gmp-devel mpfr-devel libmpc-devel
    ;;
  *)
    echo "Unsupported distro: $ID" >&2
    exit 1
    ;;
esac
