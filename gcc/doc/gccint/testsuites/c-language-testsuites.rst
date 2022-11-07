..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _c-tests:

C Language Testsuites
*********************

GCC contains the following C language testsuites, in the
:samp:`gcc/testsuite` directory:

:samp:`gcc.dg`
  This contains tests of particular features of the C compiler, using the
  more modern :samp:`dg` harness.  Correctness tests for various compiler
  features should go here if possible.

  Magic comments determine whether the file
  is preprocessed, compiled, linked or run.  In these tests, error and warning
  message texts are compared against expected texts or regular expressions
  given in comments.  These tests are run with the options :samp:`-ansi -pedantic`
  unless other options are given in the test.  Except as noted below they
  are not run with multiple optimization options.

:samp:`gcc.dg/compat`
  This subdirectory contains tests for binary compatibility using
  :samp:`lib/compat.exp`, which in turn uses the language-independent support
  (see :ref:`compat-testing`).

:samp:`gcc.dg/cpp`
  This subdirectory contains tests of the preprocessor.

:samp:`gcc.dg/debug`
  This subdirectory contains tests for debug formats.  Tests in this
  subdirectory are run for each debug format that the compiler supports.

:samp:`gcc.dg/format`
  This subdirectory contains tests of the :option:`-Wformat` format
  checking.  Tests in this directory are run with and without
  :option:`-DWIDE`.

:samp:`gcc.dg/noncompile`
  This subdirectory contains tests of code that should not compile and
  does not need any special compilation options.  They are run with
  multiple optimization options, since sometimes invalid code crashes
  the compiler with optimization.

:samp:`gcc.dg/special`
  .. todo:: describe this

:samp:`gcc.c-torture`
  This contains particular code fragments which have historically broken easily.
  These tests are run with multiple optimization options, so tests for features
  which only break at some optimization levels belong here.  This also contains
  tests to check that certain optimizations occur.  It might be worthwhile to
  separate the correctness tests cleanly from the code quality tests, but
  it hasn't been done yet.

:samp:`gcc.c-torture/compat`
  .. todo:: describe this

  This directory should probably not be used for new tests.

:samp:`gcc.c-torture/compile`
  This testsuite contains test cases that should compile, but do not
  need to link or run.  These test cases are compiled with several
  different combinations of optimization options.  All warnings are
  disabled for these test cases, so this directory is not suitable if
  you wish to test for the presence or absence of compiler warnings.
  While special options can be set, and tests disabled on specific
  platforms, by the use of :samp:`.x` files, mostly these test cases
  should not contain platform dependencies.

  .. todo:: discuss how defines such as ``STACK_SIZE`` are used

:samp:`gcc.c-torture/execute`
  This testsuite contains test cases that should compile, link and run;
  otherwise the same comments as for :samp:`gcc.c-torture/compile` apply.

:samp:`gcc.c-torture/execute/ieee`
  This contains tests which are specific to IEEE floating point.

:samp:`gcc.c-torture/unsorted`
  .. todo:: describe this

  This directory should probably not be used for new tests.

:samp:`gcc.misc-tests`
  This directory contains C tests that require special handling.  Some
  of these tests have individual expect files, and others share
  special-purpose expect files:

  :samp:`bprob*.c`
    Test :option:`-fbranch-probabilities` using
    :samp:`gcc.misc-tests/bprob.exp`, which
    in turn uses the generic, language-independent framework
    (see :ref:`profopt-testing`).

  :samp:`gcov*.c`
    Test :command:`gcov` output using :samp:`gcov.exp`, which in turn uses the
    language-independent support (see :ref:`gcov-testing`).

  :samp:`i386-pf-*.c`
    Test i386-specific support for data prefetch using :samp:`i386-prefetch.exp`.

:samp:`gcc.test-framework`

  :samp:`dg-*.c`
    Test the testsuite itself using :samp:`gcc.test-framework/test-framework.exp`.

.. todo:: merge in :samp:`testsuite/README.gcc` and discuss the format of
  test cases and magic comments more.