.. Copyright (C) 2014 Free Software Foundation, Inc.
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
     --enable-languages=jit \
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

.. option:: --enable-languages=jit

  This specifies which frontends to build.  The JIT library looks like
  a frontend to the rest of the code.

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

The test executables can be seen as:

.. code-block:: console

  jit/build/gcc/testsuite/jit/*.exe

which can be run independently.

You can compile and run individual tests by passing "jit.exp=TESTNAME" to RUNTESTFLAGS e.g.:

.. code-block:: console

   [gcc] $ make check-jit RUNTESTFLAGS="-v -v -v jit.exp=test-factorial.c"

and once a test has been compiled, you can debug it directly:

.. code-block:: console

   [gcc] $ PATH=.:$PATH \
           LD_LIBRARY_PATH=. \
           LIBRARY_PATH=. \
             gdb --args \
               testsuite/jit/test-factorial.exe

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

Overview of code structure
--------------------------

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
