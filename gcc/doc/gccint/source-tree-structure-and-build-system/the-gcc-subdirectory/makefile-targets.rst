..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: makefile targets, targets, makefile

.. _makefile:

Makefile Targets
^^^^^^^^^^^^^^^^

These targets are available from the :samp:`gcc` directory:

``all``
  This is the default target.  Depending on what your build/host/target
  configuration is, it coordinates all the things that need to be built.

``doc``
  Produce info-formatted documentation and man pages.  Essentially it
  calls :samp:`make man` and :samp:`make info`.

``pdf``
  Produce PDF-formatted documentation.

``html``
  Produce HTML-formatted documentation.

``man``
  Generate man pages.

``info``
  Generate info-formatted pages.

``mostlyclean``
  Delete the files made while building the compiler.

``clean``
  That, and all the other files built by :samp:`make all`.

``distclean``
  That, and all the files created by :command:`configure`.

``maintainer-clean``
  Distclean plus any file that can be generated from other files.  Note
  that additional tools may be required beyond what is normally needed to
  build GCC.

``srcextra``
  Generates files in the source directory that are not version-controlled but
  should go into a release tarball.

``srcinfo`` ``srcman``
  Copies the info-formatted and manpage documentation into the source
  directory usually for the purpose of generating a release tarball.

``install``
  Installs GCC.

``uninstall``
  Deletes installed files, though this is not supported.

``check``
  Run the testsuite.  This creates a :samp:`testsuite` subdirectory that
  has various :samp:`.sum` and :samp:`.log` files containing the results of
  the testing.  You can run subsets with, for example, :samp:`make check-gcc`.
  You can specify specific tests by setting :envvar:`RUNTESTFLAGS` to be the name
  of the :samp:`.exp` file, optionally followed by (for some tests) an equals
  and a file wildcard, like:

  .. code-block:: c++

    make check-gcc RUNTESTFLAGS="execute.exp=19980413-*"

  Note that running the testsuite may require additional tools be
  installed, such as Tcl or DejaGnu.

  The toplevel tree from which you start GCC compilation is not
  the GCC directory, but rather a complex Makefile that coordinates
  the various steps of the build, including bootstrapping the compiler
  and using the new compiler to build target libraries.

When GCC is configured for a native configuration, the default action
for :command:`make` is to do a full three-stage bootstrap.  This means
that GCC is built three times---once with the native compiler, once with
the native-built compiler it just built, and once with the compiler it
built the second time.  In theory, the last two should produce the same
results, which :samp:`make compare` can check.  Each stage is configured
separately and compiled into a separate directory, to minimize problems
due to ABI incompatibilities between the native compiler and GCC.

If you do a change, rebuilding will also start from the first stage
and 'bubble' up the change through the three stages.  Each stage
is taken from its build directory (if it had been built previously),
rebuilt, and copied to its subdirectory.  This will allow you to, for
example, continue a bootstrap after fixing a bug which causes the
stage2 build to crash.  It does not provide as good coverage of the
compiler as bootstrapping from scratch, but it ensures that the new
code is syntactically correct (e.g., that you did not use GCC extensions
by mistake), and avoids spurious bootstrap comparison
failuresExcept if the compiler was buggy and miscompiled
some of the files that were not modified.  In this case, it's best
to use :command:`make restrap`.

Other targets available from the top level include:

``bootstrap-lean``
  Like ``bootstrap``, except that the various stages are removed once
  they're no longer needed.  This saves disk space.

``bootstrap2`` ``bootstrap2-lean``
  Performs only the first two stages of bootstrap.  Unlike a three-stage
  bootstrap, this does not perform a comparison to test that the compiler
  is running properly.  Note that the disk space required by a 'lean'
  bootstrap is approximately independent of the number of stages.

:samp:`stage{N}-bubble ({N} = 1...4, profile, feedback)`
  Rebuild all the stages up to :samp:`{N}`, with the appropriate flags,
  'bubbling' the changes as described above.

:samp:`all-stage{N} ({N} = 1...4, profile, feedback)`
  Assuming that stage :samp:`{N}` has already been built, rebuild it with the
  appropriate flags.  This is rarely needed.

``cleanstrap``
  Remove everything (:samp:`make clean`) and rebuilds (:samp:`make bootstrap`).

``compare``
  Compares the results of stages 2 and 3.  This ensures that the compiler
  is running properly, since it should produce the same object files
  regardless of how it itself was compiled.

:samp:`distclean-stage{N} ({N} = 1...4, profile, feedback)`
  Wipe stage :samp:`{N}` and all the following ones.

  For example,
  :samp:`make distclean-stage3` wipes stage 3 and all the following ones,
  so that another :command:`make` then rebuilds them from scratch.
  This can be useful if you're doing changes where
  'bubbling' the changes as described above is not sufficient,
  but a full :command:`make restrap` isn't necessary either.

``profiledbootstrap``
  Builds a compiler with profiling feedback information.  In this case,
  the second and third stages are named :samp:`profile` and :samp:`feedback`,
  respectively.  For more information, see the installation instructions.

``restrap``
  Restart a bootstrap, so that everything that was not built with
  the system compiler is rebuilt.

:samp:`stage{N}-start ({N} = 1...4, profile, feedback)`
  For each package that is bootstrapped, rename directories so that,
  for example, :samp:`gcc` points to the stage :samp:`{N}` GCC, compiled
  with the stage :samp:`{N-1}` GCCCustomarily, the system compiler
  is also termed the :samp:`stage0` GCC.

  .

  You will invoke this target if you need to test or debug the
  stage :samp:`{N}` GCC.  If you only need to execute GCC (but you need
  not run :samp:`make` either to rebuild it or to run test suites),
  you should be able to work directly in the :samp:`stage{N}-gcc`
  directory.  This makes it easier to debug multiple stages in
  parallel.

``stage``
  For each package that is bootstrapped, relocate its build directory
  to indicate its stage.  For example, if the :samp:`gcc` directory
  points to the stage2 GCC, after invoking this target it will be
  renamed to :samp:`stage2-gcc`.

  If you wish to use non-default GCC flags when compiling the stage2 and
  stage3 compilers, set ``BOOT_CFLAGS`` on the command line when doing
  :samp:`make`.

Usually, the first stage only builds the languages that the compiler
is written in: typically, C and maybe Ada.  If you are debugging a
miscompilation of a different stage2 front-end (for example, of the
Fortran front-end), you may want to have front-ends for other languages
in the first stage as well.  To do so, set ``STAGE1_LANGUAGES``
on the command line when doing :samp:`make`.

For example, in the aforementioned scenario of debugging a Fortran
front-end miscompilation caused by the stage1 compiler, you may need a
command like

.. code-block:: c++

  make stage2-bubble STAGE1_LANGUAGES=c,fortran

Alternatively, you can use per-language targets to build and test
languages that are not enabled by default in stage1.  For example,
:command:`make f951` will build a Fortran compiler even in the stage1
build directory.
