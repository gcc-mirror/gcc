..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _lto-testing:

Support for testing link-time optimizations
*******************************************

Tests for link-time optimizations usually require multiple source files
that are compiled separately, perhaps with different sets of options.
There are several special-purpose test directives used for these tests.

:samp:`{ dg-lto-do {do-what-keyword} }`
  :samp:`{do-what-keyword}` specifies how the test is compiled and whether
  it is executed.  It is one of:

  ``assemble``
    Compile with :option:`-c` to produce a relocatable object file.

  ``link``
    Compile, assemble, and link to produce an executable file.

  ``run``
    Produce and run an executable file, which is expected to return
    an exit code of 0.

  The default is ``assemble``.  That can be overridden for a set of
  tests by redefining ``dg-do-what-default`` within the ``.exp``
  file for those tests.

  Unlike ``dg-do``, ``dg-lto-do`` does not support an optional
  :samp:`target` or :samp:`xfail` list.  Use ``dg-skip-if``,
  ``dg-xfail-if``, or ``dg-xfail-run-if``.

:samp:`{ dg-lto-options { { {options} } [{ {options} }] } [{ target {selector} }]}`
  This directive provides a list of one or more sets of compiler options
  to override :samp:`{LTO_OPTIONS}`.  Each test will be compiled and run with
  each of these sets of options.

:samp:`{ dg-extra-ld-options {options} [{ target {selector} }]}`
  This directive adds :samp:`{options}` to the linker options used.

:samp:`{ dg-suppress-ld-options {options} [{ target {selector} }]}`
  This directive removes :samp:`{options}` from the set of linker options used.
