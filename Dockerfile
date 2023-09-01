FROM debian:12 AS gcc-builder

RUN apt-get update; \
    DEBIAN_FRONTEND="noninteractive" apt-get install -y --no-install-recommends \
    autoconf \
    automake \
    bzip2\
    dpkg-dev \
    file \
    wget \
    curl \
    build-essential \
    imagemagick \
    libbz2-dev \
    libc6-dev \
    libcurl4-openssl-dev \
    libdb-dev \
    libevent-dev \
    libffi-dev \
    libgdbm-dev \
    libglib2.0-dev \
    libgmp-dev \
    libjpeg-dev \
    libkrb5-dev \
    liblzma-dev \
    libmagickcore-dev \
    libmagickwand-dev \
    libmaxminddb-dev \
    libncurses5-dev \
    libncursesw5-dev \
    libpng-dev \
    libpq-dev \
    libreadline-dev \
    libsqlite3-dev \
    libssl-dev \
    libtool \
    libwebp-dev \
    libxml2-dev \
    libxslt-dev \
    libyaml-dev \
    make \
    patch \
    unzip \
    xz-utils \
    zlib1g-dev \
    flex \
    bison \
    git

ADD . /usr/src/gcc
RUN /bin/sh -c set -ex; \
    cd /usr/src/gcc; \
    git log -1 --format="%h" > /GCCRS_BUILD; \
    ./contrib/download_prerequisites; 	{ rm *.tar.* || true; }; \
    mkdir -p /usr/src/gcc/gcc-build; \
    cd /usr/src/gcc/gcc-build; \
    /usr/src/gcc/configure --disable-bootstrap --disable-multilib --enable-languages=rust; \
    make -j "$(nproc)"; \
    make install-strip; \
    cd /root; \
    rm -rf /usr/src/gcc

RUN /bin/sh -c set -ex; \
    echo '/usr/local/lib64' > /etc/ld.so.conf.d/local-lib64.conf; \
    ldconfig -v

FROM rust:latest
COPY --from=gcc-builder /usr/ /usr/
COPY --from=gcc-builder /GCCRS_BUILD /GCCRS_BUILD
RUN cargo install --git https://github.com/Rust-GCC/cargo-gccrs cargo-gccrs

CMD ["bash"]
