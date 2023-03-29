![C/C++ CI](https://github.com/Rust-GCC/gccrs/workflows/C/C++%20CI/badge.svg)
[![GCC Bootstrap Build](https://github.com/Rust-GCC/gccrs/actions/workflows/bootstrap.yml/badge.svg)](https://github.com/Rust-GCC/gccrs/actions/workflows/bootstrap.yml)
[![Build Docker image](https://github.com/Rust-GCC/gccrs/actions/workflows/docker.yml/badge.svg)](https://github.com/Rust-GCC/gccrs/actions/workflows/docker.yml)
![Docker Pulls](https://img.shields.io/docker/pulls/philberty/gccrs)
[![project chat](https://img.shields.io/badge/zulip-join_chat-brightgreen.svg)](https://gcc-rust.zulipchat.com/)
[![Bors enabled](https://bors.tech/images/badge_small.svg)](https://app.bors.tech/repositories/32890)
# GCC Rust
![GCC Rust](logo.png?raw=true "GCC rust Logo")

Please note, the compiler is in a very early stage and not usable yet for compiling real Rust programs.

gccrs is a full alternative implementation of the Rust language ontop of GCC with the goal
to become fully upstream with the GNU toolchain.

The origin of this project was a community effort several years ago where Rust was still at version 0.9;
the language was subject to so much change that it became difficult for a community effort to play catch up.
Now that the language is stable, it is an excellent time to create alternative compilers. The developers of
the project are keen “Rustaceans” with a desire to give back to the Rust community and to learn what GCC is capable
of when it comes to a modern language.

## Build Farm Status

- [Debian i386](https://builder.sourceware.org/buildbot/#/builders/gccrust-debian-i386) [![Debian i386](https://builder.sourceware.org/buildbot/badges/gccrust-debian-i386.svg)](https://builder.sourceware.org/buildbot/#/builders/gccrust-debian-i386)
- [Debian ppc64](https://builder.sourceware.org/buildbot/#/builders/gccrust-debian-ppc64) [![Debian ppc64](https://builder.sourceware.org/buildbot/badges/gccrust-debian-ppc64.svg)](https://builder.sourceware.org/buildbot/#/builders/gccrust-debian-ppc64)
- [Debian testing-x86_64](https://builder.sourceware.org/buildbot/#/builders/146) [![Debian testing-x86_64](https://builder.sourceware.org/buildbot/badges/gccrust-debian-testing-x86_64.svg)](https://builder.sourceware.org/buildbot/#/builders/146)
- [Fedora arm64](https://builder.sourceware.org/buildbot/#/builders/179) [![Fedora arm64](https://builder.sourceware.org/buildbot/badges/gccrust-fedora-arm64.svg)](https://builder.sourceware.org/buildbot/#/builders/179)
- [Fedora ppc64le](https://builder.sourceware.org/buildbot/#/builders/gccrust-fedora-ppc64le) [![Fedora ppc64le](https://builder.sourceware.org/buildbot/badges/gccrust-fedora-ppc64le.svg)](https://builder.sourceware.org/buildbot/#/builders/gccrust-fedora-ppc64le)
- [Fedora s390x](https://builder.sourceware.org/buildbot/#/builders/gccrust-fedora-s390x) [![Fedora s390x](https://builder.sourceware.org/buildbot/badges/gccrust-fedora-s390x.svg)](https://builder.sourceware.org/buildbot/#/builders/gccrust-fedora-s390x)
- [Fedora X86_64](https://builder.sourceware.org/buildbot/#/builders/gccrust-fedora-x86_64) [![Fedora X86-64](https://builder.sourceware.org/buildbot/badges/gccrust-fedora-x86_64.svg)](https://builder.sourceware.org/buildbot/#/builders/gccrust-fedora-x86_64)
- [OpenSUSE Leap X86_64](https://builder.sourceware.org/buildbot/#/builders/104) [![OpenSUSE Leap X86_64](https://builder.sourceware.org/buildbot/badges/gccrust-opensuseleap-x86_64.svg)](https://builder.sourceware.org/buildbot/#/builders/104)
- [OpenSUSE tw X86_64](https://builder.sourceware.org/buildbot/#/builders/103) [![OpenSUSE tw X86_64](https://builder.sourceware.org/buildbot/badges/gccrust-opensusetw-x86_64.svg)](https://builder.sourceware.org/buildbot/#/builders/103)
- [Rawhide X86_64](https://builder.sourceware.org/buildbot/#/builders/132) [![Rawhide X86_64](https://builder.sourceware.org/buildbot/badges/gccrust-rawhide-x86_64.svg)](https://builder.sourceware.org/buildbot/#/builders/132)

## FAQ

Please find the answers to frequently asked questions over on: https://github.com/Rust-GCC/gccrs/wiki/Frequently-Asked-Questions

## Development Environment

### Building

Fetch dependencies for Ubuntu:

```bash
$ apt install build-essential libgmp3-dev libmpfr-dev libmpc-dev flex bison autogen gcc-multilib dejagnu
```

Clone the repository

```bash
$ git clone https://github.com/Rust-GCC/gccrs
```

#### Linux

It is important to remember that GNU toolchain projects are designed to be built outside of their source directory
which is why a build directory is created.

```bash
$ mkdir gccrs-build
$ cd gccrs-build
$ ../gccrs/configure --prefix=$HOME/gccrs-install --disable-bootstrap --enable-multilib --enable-languages=rust
$ make
```

#### MacOS

The path of header dir and sysroot should be specified when you configure the project.
```bash
$ mkdir mac-build
$ cd mac-build
$ ../gccrs/configure --prefix=$HOME/gccrs-install --disable-bootstrap --enable-multilib --enable-languages=rust --with-native-system-header-dir=/usr/include --with-sysroot=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk
$ make

```

#### Running GCC Rust

Running the compiler itself without make install we can simply invoke the compiler proper:

```bash
$ ./gcc/rust1 test.rs -frust-debug -frust-dump-parse -Warray-bounds -dumpbase test.rs -mtune=generic -march=x86-64 -O0 -version -fdump-tree-gimple -o test.s -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64
```

To invoke the compiler driver (gccrs) we need to:

```bash
$ make install
```

Then invoke the compiler from the installation directory:

```bash
$ $HOME/gccrs-install/gccrs -g -O2 -c test.rs -o test.o
$ $HOME/gccrs-install/gccrs -o test test.o
```

You can also setup your shell to automatically find the installed compiler. For example for `bash`,
add the following in your `$HOME/.bashrc`:

```bash
export PATH=$HOME/gccrs-install/bin:$PATH

```

## Testsuite

Invoke the full testsuite from the build directory (`gccrs-build` in the previous commands):

```bash
$ make check-rust
```

Invoke a subset of the testsuite. For example, to only run tests that are currently known/expected to fail:

```bash
$ make check-rust RUNTESTFLAGS="xfail.exp"
```
There are the following sets of tests:
- `compile.exp` : compilation tests
- `execute.exp` : execution tests
- `xfail.exp` : tests that are currently known/expected to fail

Invoke only a specific test :

```bash
$ make check-rust RUNTESTFLAGS="--all compile.exp=continue1.rs"
```

Logs (with corresponding commands) can be found in : `gccrs-build/gcc/testsuite/rust/rust.log`.

See [GCC Testing documentation](https://gcc.gnu.org/install/test.html) for more details.

Test cases are located within [`gcc/testsuite/rust/`](gcc/testsuite/rust/).
Please contribute your specific
test cases referencing any issues on Github.

## Debugging

### Enabling internal checks

GCC has several internal checks that can be enabled during configuration. In the case of `gccrs`,
you can enable the following:
```bash
$ ../gccrs/configure --prefix=$HOME/gccrs-install --disable-bootstrap --enable-multilib --enable-languages=rust --enable-checking=gimple,tree,types
```

### GDB
You can directly invoke `gdb` on the `rust1` compiler process (you can find the
exact command adding `--verbose` to your `gccrs` invocation):
```bash
$ gccrs test.rs -O0 -S -o arithmetic_expressions1.s --verbose
...
 /some/path/../../rust1 test.rs -quiet -dumpbase arithmetic_expressions1.rs -dumpbase-ext .rs
 -mtune=generic -march=x86-64 -O0 -w -version -fdiagnostics-color=never -fno-diagnostics-show-caret -fno-diagnostics-show-line-numbers -fdiagnostics-urls=never -fdiagnostics-path-format=separate-events -o test.s -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu
...
$ gdb --args  /some/path/../../rust1 test.rs -quiet -dumpbase arithmetic_expressions1.rs -dumpbase-ext .rs
 -mtune=generic -march=x86-64 -O0 -w -version -fdiagnostics-color=never -fno-diagnostics-show-caret -fno-diagnostics-show-line-numbers -fdiagnostics-urls=never -fdiagnostics-path-format=separate-events -o test.s -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu
```

Or simply add the `-wrapper gdb,--args` option.
This will call each subcommand in `gdb` and you simply have to break/debug in `rust1`:
```bash
$ gccrs test.rs -O0 -S -o arithmetic_expressions1.s -wrapper gdb,--args
```

## Docker image

There is a docker image hosted over on:

https://hub.docker.com/repository/docker/philberty/gccrs

```bash
$ docker pull philberty/gccrs
```

Or you can build your own image:

```bash
$ docker build . -t gccrs-dev
```
If you want to build an object file:

```bash
$ docker run --rm -v "$PWD":/usr/src/myapp -w /usr/src/myapp \
    gccrs-dev:latest gccrs -g -O2 -c \
    gcc/testsuite/rust/compile/torture/type_infer1.rs -o type_infer1.o
```

If you want to build an executable file:
```bash
$ docker run --rm -v "$PWD":/usr/src/myapp -w /usr/src/myapp \
    gccrs-dev:latest gccrs -g -O2 \
    gcc/testsuite/rust/compile/torture/type_infer1.rs -o type_infer1
```

To emit assembly :
```bash
$ docker run --rm -v "$PWD":/usr/src/myapp -w /usr/src/myapp \
    gccrs-dev:latest gccrs -g -O2 \
    gcc/testsuite/rust/compile/torture/type_infer1.rs -S -o type_infer1.s
```

To emit Rust front end debug output, you may add options like `-frust-debug`, `-frust-dump-all`.


## Contributing

If you want to contribute to GCC Rust, you can find more information in [CONTRIBUTING.md](https://github.com/Rust-GCC/gccrs/blob/master/CONTRIBUTING.md).

Please be aware this project is designed to be pushed upstream to GCC when we reach some milestones,
and this means we require copyright assignment or the Developer's Certificate of Origin sign-off.
Please see the [Contributing to GCC](https://gcc.gnu.org/contribute.html) guide or [Developer's Certificate of Origin (DCO) Sign-off](https://gcc.gnu.org/dco.html) guide.

Not all contributions must be code; we would love to see new test cases or bugs and issues to be reported.
Feel free to add any comments on open PRs


## Continuous Integration

When submitting (or updating) a [GitHub Pull Request](https://github.com/Rust-GCC/gccrs/pull/),
several automated checks are run.
Generally, a "green status" is necessary before merge.


### Compiler Diagnostics

That is, here, diagnostics emitted by the "initial" compiler used to build GCC/Rust.

If building a native toolchain,
GCC by default does a 3-stage bootstrap build (<https://gcc.gnu.org/install/configure.html>).
In addition to making sure that GCC is able to reproduce itself bit-by-bit,
this also means that stages 2+ are built with `-Werror`
(turning most _warning_ into _error_ diagnostics; see `--enable-werror`,
possibly enabled by default).
This helps to catch a good number of bugs, because it enforces that GCC compiles without compiler diagnostics;
it's a requirement for upstream patch submission (<https://gcc.gnu.org/contribute.html#testing>).

GCC generally is only expected to be "warning-clean" without `--disable-bootstrap`
(that is, default `--enable-bootstrap` for a native build),
and not for the initial stage where it's using the "initial" compiler -- otherwise
we're at the mercy of whatever "initial" compiler we're using.
Doing a `--disable-bootstrap` build is much faster, of course, so we're often doing that:
for example, per the instructions above, or in the standard CI.
With that, we're missing out on the aspect that _enforces that GCC compiles without compiler diagnostics_.

To encounter that, the default CI has a [_check for new warnings_ step](https://github.com/Rust-GCC/gccrs/pull/1026)
that verifies in the CI `--disable-bootstrap` build configuration that no new warnings are introduced.
If that step fails, it usually points out a new _warning_ you've introduced erroneously, and should address.
Occasionally it means that simply the `.github/bors_log_expected_warnings` file needs to be updated,
for example if due to any kind of "environmental changes" (for example, CI "initial" compiler changes).
Unless diligently reproducing the CI configuration (in particular "initial" compiler, GCC version),
it's not really feasible to reproduce this check locally.
If in doubt, do a local `--enable-bootstrap` build, or submit your changes, and wait for the CI system's results.


## Community

We can be found on all usual Rust channels such as Zulip, but we also have our own channels:

* GCC Rust Zulip: https://gcc-rust.zulipchat.com/
* Twitter: https://twitter.com/gcc_rust
* GCC Mailing List: https://gcc.gnu.org/mailman/listinfo/gcc-rust
* irc: irc.oftc.net - gccrust
