![C/C++ CI](https://github.com/philberty/gccrs/workflows/C/C++%20CI/badge.svg)
![Docker Build](https://img.shields.io/docker/cloud/build/philberty/gccrs)
![Docker Pulls](https://img.shields.io/docker/pulls/philberty/gccrs)
[![project chat](https://img.shields.io/badge/zulip-join_chat-brightgreen.svg)](https://gcc-rust.zulipchat.com/)
# GCC Rust
![GCC Rust](logo.png?raw=true "GCC rust Logo")

This is a full alternative implementaion of the Rust language ontop of GCC which the goal
to become fully upstream with the GNU toolchain.

The origin of this project was a community effort several years ago where Rust was still at version 0.9;
the language was subject to so much change that it became difficult for a community effort to play catch up.
Now that the language is in a stable state, it is a good time to create alternative compilers. The developers of
the project are keen “Rustaceans” with a desire to give back to the Rust community and to learn what GCC is capable
of when it comes to a modern language.

## Development Enviroment

Fetch dependancies for ubuntu:

```bash
$ apt install build-essential libgmp3-dev libmpfr-dev libmpc-dev flex bison autogen gcc-multilib dejagnu
```

Clone the repository

```bash
$ git clone git@github.com:Rust-GCC/gccrs.git
```

Compilation script. It is important to remember that GNU toolchain projects are designed to be built outside of its source directory
this is why a build directory is created.

```bash
$ mkdir gccrs-build
$ cd gccrs-build
$ ../gccrs/configure --prefix=$HOME/gccrs-install --disable-bootstrap --enable-multilib --enable-languages=rust
$ make
```

Running the compiler itself without make install we can simply invoke the compiler proper:

```
$ gdb --args ./gcc/rust1 test.rs -frust-dump-parse -Warray-bounds -dumpbase test.rs -mtune=generic -march=x86-64 -O0 -version -fdump-tree-gimple -o test.s -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64
```

To invoking the compiler driver (gccrs) we need to:

```
$ make install
```

Then invoke the compiler as expected:

```
$ gccrs -g -O2 -c test.rs -o test.o
$ gccrs -o test test.o
```

## Testsuite

The test suite can be invoked via:

```
$ make check-rust
```

Test cases can be found within gcc/testsuite/rust.test please feel free to contribute your specific
test cases referencing any issues on github.

## Docker image

There is a docker image hosted over on: 

https://hub.docker.com/repository/docker/philberty/gccrs

```
$ docker pull philberty/gccrs
```

Or you can build your own image:

```
$ docker build . -t gccrs-dev
$ docker run --rm -v "$PWD":/usr/src/myapp -w /usr/src/myapp \
    gccrs-dev:latest gccrs -g -O2 -c \
    gcc/testsuite/rust.test/compilable/type_infer1.rs -o type_infer1.o
```

## Contributing

Please be aware this project is designed to be pushed upstream to GCC when we reach some milestones and this means we require
contribtions to have copyright assignment in place. Please see: https://gcc.gnu.org/contribute.html

Not all contributions must be code, we would love to see new test cases or bugs and issues to be reported. Feel free to add any comments on open PRs

## Community

We can be found on all usual Rust channels such as Zulip but we also have our own channels:

 * GCC Rust Zulip: https://gcc-rust.zulipchat.com/
 * Twitter: https://twitter.com/gcc_rust
