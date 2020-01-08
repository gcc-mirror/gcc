.. Copyright (C) 2014-2020 Free Software Foundation, Inc.
   Originally contributed by David Malcolm <dmalcolm@redhat.com>

   This is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see
   <http://www.gnu.org/licenses/>.

Internals
=========

Working on the JIT library
--------------------------
Having checked out the source code (to "src"), you can configure and build
the JIT library like this:

.. code-block:: bash

  mkdir build
  mkdir install
  PREFIX=$(pwd)/install
  cd build
  ../src/configure \
     --enable-host-shared \
     --enable-languages=jit,c++ \
     --disable-bootstrap \
     --enable-checking=release \
     --prefix=$PREFIX
  nice make -j4 # altering the "4" to however many cores you have

This should build a libgccjit.so within jit/build/gcc:

.. code-block:: console

 [build] $ file gcc/libgccjit.so*
 gcc/libgccjit.so:       symbolic link to `libgccjit.so.0'
 gcc/libgccjit.so.0:     symbolic link to `libgccjit.so.0.0.1'
 gcc/libgccjit.so.0.0.1: ELF 64-bit LSB shared object, x86-64, version 1 (SYSV), dynamically linked, not stripped

Here's what those configuration options mean:

.. option:: --enable-host-shared

  Configuring with this option means that the compiler is built as
  position-independent code, which incurs a slight performance hit,
  but it necessary for a shared library.

.. option:: --enable-languages=jit,c++

  This specifies which frontends to build.  The JIT library looks like
  a frontend to the rest of the code.

  The C++ portion of the JIT test suite requires the C++ frontend to be
  enabled at configure-time, or you may see errors like this when
  running the test suite:

  .. code-block:: console

    xgcc: error: /home/david/jit/src/gcc/testsuite/jit.dg/test-quadratic.cc: C++ compiler not installed on this system
    c++: error trying to exec 'cc1plus': execvp: No such file or directory

.. option:: --disable-bootstrap

  For hacking on the "jit" subdirectory, performing a full
  bootstrap can be overkill, since it's unused by a bootstrap.  However,
  when submitting patches, you should remove this option, to ensure that
  the compiler can still bootstrap itself.

.. option:: --enable-checking=release

  The compile can perform extensive self-checking as it runs, useful when
  debugging, but slowing things down.

  For maximum speed, configure with ``--enable-checking=release`` to
  disable this self-checking.

Running the test suite
----------------------

.. code-block:: console

  [build] $ cd gcc
  [gcc] $ make check-jit RUNTESTFLAGS="-v -v -v"

A summary of the tests can then be seen in:

.. code-block:: console

  jit/build/gcc/testsuite/jit/jit.sum

and detailed logs in:

.. code-block:: console

  jit/build/gcc/testsuite/jit/jit.log

The test executables are normally deleted after each test is run.  For
debugging, they can be preserved by setting :envvar:`PRESERVE_EXECUTABLES`
in the environment.  If so, they can then be seen as:

.. code-block:: console

  jit/build/gcc/testsuite/jit/*.exe

which can be run independently.

You can compile and run individual tests by passing "jit.exp=TESTNAME" to RUNTESTFLAGS e.g.:

.. code-block:: console

   [gcc] $ PRESERVE_EXECUTABLES= \
             make check-jit \
               RUNTESTFLAGS="-v -v -v jit.exp=test-factorial.c"

and once a test has been compiled, you can debug it directly:

.. code-block:: console

   [gcc] $ PATH=.:$PATH \
           LD_LIBRARY_PATH=. \
           LIBRARY_PATH=. \
             gdb --args \
               testsuite/jit/test-factorial.c.exe

Running under valgrind
**********************

The jit testsuite detects if :envvar:`RUN_UNDER_VALGRIND` is present in the
environment (with any value).  If it is present, it runs the test client
code under `valgrind <http://valgrind.org>`_,
specifcally, the default
`memcheck <http://valgrind.org/docs/manual/mc-manual.html>`_
tool with
`--leak-check=full
<http://valgrind.org/docs/manual/mc-manual.html#opt.leak-check>`_.

It automatically parses the output from valgrind, injecting XFAIL results if
any issues are found, or PASS results if the output is clean.  The output
is saved to ``TESTNAME.exe.valgrind.txt``.

For example, the following invocation verbosely runs the testcase
``test-sum-of-squares.c`` under valgrind, showing an issue:

.. code-block:: console

  $ RUN_UNDER_VALGRIND= \
      make check-jit \
        RUNTESTFLAGS="-v -v -v jit.exp=test-sum-of-squares.c"

  (...verbose log contains detailed valgrind errors, if any...)

                  === jit Summary ===

  # of expected passes            28
  # of expected failures          2

  $ less testsuite/jit/jit.sum
  (...other results...)
  XFAIL: jit.dg/test-sum-of-squares.c: test-sum-of-squares.c.exe.valgrind.txt: definitely lost: 8 bytes in 1 blocks
  XFAIL: jit.dg/test-sum-of-squares.c: test-sum-of-squares.c.exe.valgrind.txt: unsuppressed errors: 1
  (...other results...)

  $ less testsuite/jit/test-sum-of-squares.c.exe.valgrind.txt
  (...shows full valgrind report for this test case...)

When running under valgrind, it's best to have configured gcc with
:option:`--enable-valgrind-annotations`, which automatically suppresses
various known false positives.

Environment variables
---------------------
When running client code against a locally-built libgccjit, three
environment variables need to be set up:

.. envvar:: LD_LIBRARY_PATH

   `libgccjit.so` is dynamically linked into client code, so if running
   against a locally-built library, ``LD_LIBRARY_PATH`` needs to be set
   up appropriately.  The library can be found within the "gcc"
   subdirectory of the build tree:

  .. code-block:: console

    $ file libgccjit.so*
    libgccjit.so:       symbolic link to `libgccjit.so.0'
    libgccjit.so.0:     symbolic link to `libgccjit.so.0.0.1'
    libgccjit.so.0.0.1: ELF 64-bit LSB shared object, x86-64, version 1 (GNU/Linux), dynamically linked, not stripped

.. envvar:: PATH

  The library uses a driver executable for converting from .s assembler
  files to .so shared libraries.  Specifically, it looks for a name
  expanded from
  ``${target_noncanonical}-gcc-${gcc_BASEVER}${exeext}``
  such as ``x86_64-unknown-linux-gnu-gcc-5.0.0``.

  Hence ``PATH`` needs to include a directory where the library can
  locate this executable.

  The executable is normally installed to the installation bindir
  (e.g. /usr/bin), but a copy is also created within the "gcc"
  subdirectory of the build tree for running the testsuite, and for ease
  of development.

.. envvar:: LIBRARY_PATH

  The driver executable invokes the linker, and the latter needs to locate
  support libraries needed by the generated code, or you will see errors
  like:

  .. code-block:: console

    ld: cannot find crtbeginS.o: No such file or directory
    ld: cannot find -lgcc
    ld: cannot find -lgcc_s

  Hence if running directly from a locally-built copy (without installing),
  ``LIBRARY_PATH`` needs to contain the "gcc" subdirectory of the build
  tree.

For example, to run a binary that uses the library against a non-installed
build of the library in LIBGCCJIT_BUILD_DIR you need an invocation of the
client code like this, to preprend the dir to each of the environment
variables:

.. code-block:: console

  $ LD_LIBRARY_PATH=$(LIBGCCJIT_BUILD_DIR):$(LD_LIBRARY_PATH) \
    PATH=$(LIBGCCJIT_BUILD_DIR):$(PATH) \
    LIBRARY_PATH=$(LIBGCCJIT_BUILD_DIR):$(LIBRARY_PATH) \
      ./jit-hello-world
  hello world

Packaging notes
---------------
The configure-time option :option:`--enable-host-shared` is needed when
building the jit in order to get position-independent code.  This will
slow down the regular compiler by a few percent.  Hence when packaging gcc
with libgccjit, please configure and build twice:

  * once without :option:`--enable-host-shared` for most languages, and

  * once with :option:`--enable-host-shared` for the jit

For example:

.. code-block:: bash

  # Configure and build with --enable-host-shared
  # for the jit:
  mkdir configuration-for-jit
  pushd configuration-for-jit
  $(SRCDIR)/configure \
    --enable-host-shared \
    --enable-languages=jit \
    --prefix=$(DESTDIR)
  make
  popd

  # Configure and build *without* --enable-host-shared
  # for maximum speed:
  mkdir standard-configuration
  pushd standard-configuration
  $(SRCDIR)/configure \
    --enable-languages=all \
    --prefix=$(DESTDIR)
  make
  popd

  # Both of the above are configured to install to $(DESTDIR)
  # Install the configuration with --enable-host-shared first
  # *then* the one without, so that the faster build
  # of "cc1" et al overwrites the slower build.
  pushd configuration-for-jit
  make install
  popd

  pushd standard-configuration
  make install
  popd

Overview of code structure
--------------------------

The library is implemented in C++.  The source files have the ``.c``
extension for legacy reasons.

* ``libgccjit.c`` implements the API entrypoints.  It performs error
  checking, then calls into classes of the gcc::jit::recording namespace
  within ``jit-recording.c`` and ``jit-recording.h``.

* The gcc::jit::recording classes (within ``jit-recording.c`` and
  ``jit-recording.h``) record the API calls that are made:

   .. literalinclude:: ../../jit-common.h
    :start-after: /* Recording types.  */
    :end-before: /* End of recording types. */
    :language: c++

* When the context is compiled, the gcc::jit::playback classes (within
  ``jit-playback.c`` and ``jit-playback.h``) replay the API calls
  within langhook:parse_file:

   .. literalinclude:: ../../jit-common.h
    :start-after: /* Playback types.  */
    :end-before: /* End of playback types. */
    :language: c++

   .. literalinclude:: ../../notes.txt
    :lines: 1-

Here is a high-level summary from ``jit-common.h``:

.. include:: ../../jit-common.h
  :start-after: This comment is included by the docs.
  :end-before: End of comment for inclusion in the docs.  */

.. _example-of-log-file:

Another way to understand the structure of the code is to enable logging,
via :c:func:`gcc_jit_context_set_logfile`.  Here is an example of a log
generated via this call:

.. literalinclude:: test-hello-world.exe.log.txt
    :lines: 1-

Design notes
------------
It should not be possible for client code to cause an internal compiler
error.  If this *does* happen, the root cause should be isolated (perhaps
using :c:func:`gcc_jit_context_dump_reproducer_to_file`) and the cause
should be rejected via additional checking.  The checking ideally should
be within the libgccjit API entrypoints in libgccjit.c, since this is as
close as possible to the error; failing that, a good place is within
``recording::context::validate ()`` in jit-recording.c.

Submitting patches
------------------
Please read the contribution guidelines for gcc at
https://gcc.gnu.org/contribute.html.

Patches for the jit should be sent to both the
gcc-patches@gcc.gnu.org and jit@gcc.gnu.org mailing lists,
with "jit" and "PATCH" in the Subject line.

You don't need to do a full bootstrap for code that just touches the
``jit`` and ``testsuite/jit.dg`` subdirectories.  However, please run
``make check-jit`` before submitting the patch, and mention the results
in your email (along with the host triple that the tests were run on).

A good patch should contain the information listed in the
gcc contribution guide linked to above; for a ``jit`` patch, the patch
shold contain:

  * the code itself (for example, a new API entrypoint will typically
    touch ``libgccjit.h`` and ``.c``, along with support code in
    ``jit-recording.[ch]`` and ``jit-playback.[ch]`` as appropriate)

  * test coverage

  * documentation for the C API

  * documentation for the C++ API

A patch that adds new API entrypoints should also contain:

  * a feature macro in ``libgccjit.h`` so that client code that doesn't
    use a "configure" mechanism can still easily detect the presence of
    the entrypoint.  See e.g. ``LIBGCCJIT_HAVE_SWITCH_STATEMENTS`` (for
    a category of entrypoints) and
    ``LIBGCCJIT_HAVE_gcc_jit_context_set_bool_allow_unreachable_blocks``
    (for an individual entrypoint).

  * a new ABI tag containing the new symbols (in ``libgccjit.map``), so
    that we can detect client code that uses them

  * Support for :c:func:`gcc_jit_context_dump_reproducer_to_file`.  Most
    jit testcases attempt to dump their contexts to a .c file; ``jit.exp``
    then sanity-checks the generated c by compiling them (though
    not running them).   A new API entrypoint
    needs to "know" how to write itself back out to C (by implementing
    ``gcc::jit::recording::memento::write_reproducer`` for the appropriate
    ``memento`` subclass).

  * C++ bindings for the new entrypoints (see ``libgccjit++.h``); ideally
    with test coverage, though the C++ API test coverage is admittedly
    spotty at the moment

  * documentation for the new C entrypoints

  * documentation for the new C++ entrypoints

  * documentation for the new ABI tag (see ``topics/compatibility.rst``).

Depending on the patch you can either extend an existing test case, or
add a new test case.  If you add an entirely new testcase: ``jit.exp``
expects jit testcases to begin with ``test-``, or ``test-error-`` (for a
testcase that generates an error on a :c:type:`gcc_jit_context`).

Every new testcase that doesn't generate errors should also touch
``gcc/testsuite/jit.dg/all-non-failing-tests.h``:

  * Testcases that don't generate errors should ideally be added to the
    ``testcases`` array in that file; this means that, in addition
    to being run standalone, they also get run within
    ``test-combination.c`` (which runs all successful tests inside one
    big :c:type:`gcc_jit_context`), and ``test-threads.c`` (which runs all
    successful tests in one process, each one running in a different
    thread on a different :c:type:`gcc_jit_context`).

    .. note::

       Given that exported functions within a :c:type:`gcc_jit_context`
       must have unique names, and most testcases are run within
       ``test-combination.c``, this means that every jit-compiled test
       function typically needs a name that's unique across the entire
       test suite.

  * Testcases that aren't to be added to the ``testcases`` array should
    instead add a comment to the file clarifying why they're not in that
    array. See the file for examples.

Typically a patch that touches the .rst documentation will also need the
texinfo to be regenerated.  You can do this with
`Sphinx 1.0 <http://sphinx-doc.org/>`_ or later by
running ``make texinfo`` within ``SRCDIR/gcc/jit/docs``.   Don't do this
within the patch sent to the mailing list; it can often be relatively
large and inconsequential (e.g. anchor renumbering), rather like generated
"configure" changes from configure.ac.  You can regenerate it when
committing to svn.
