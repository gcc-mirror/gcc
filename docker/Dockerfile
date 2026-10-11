# Runtime image for the GCC built by .github/workflows/build.yml.
# The build context is the DESTDIR staging tree (dist/), which contains opt/gcc.
# Base must match the runner OS (ubuntu-latest = 24.04) so glibc/shared libs line up.
FROM ubuntu:24.04

RUN apt-get update && apt-get install -y --no-install-recommends \
      binutils libc6-dev libgmp10 libmpfr6 libmpc3 zlib1g \
    && rm -rf /var/lib/apt/lists/*

COPY opt/gcc /opt/gcc

ENV PATH=/opt/gcc/bin:$PATH \
    LD_LIBRARY_PATH=/opt/gcc/lib64

CMD ["g++", "--version"]
