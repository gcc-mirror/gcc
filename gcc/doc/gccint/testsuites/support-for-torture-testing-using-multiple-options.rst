..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _torture-tests:

Support for torture testing using multiple options
**************************************************

Throughout the compiler testsuite there are several directories whose
tests are run multiple times, each with a different set of options.
These are known as torture tests.
:samp:`lib/torture-options.exp` defines procedures to
set up these lists:

``torture-init``
  Initialize use of torture lists.

``set-torture-options``
  Set lists of torture options to use for tests with and without loops.
  Optionally combine a set of torture options with a set of other
  options, as is done with Objective-C runtime options.

``torture-finish``
  Finalize use of torture lists.

The :samp:`.exp` file for a set of tests that use torture options must
include calls to these three procedures if:

* It calls ``gcc-dg-runtest`` and overrides :samp:`{DG_TORTURE_OPTIONS}`.

* It calls :samp:`{${tool}}` ``-torture`` or
  :samp:`{${tool}}` ``-torture-execute``, where :samp:`{tool}` is ``c``,
  ``fortran``, or ``objc``.

* It calls ``dg-pch``.

It is not necessary for a :samp:`.exp` file that calls ``gcc-dg-runtest``
to call the torture procedures if the tests should use the list in
:samp:`{DG_TORTURE_OPTIONS}` defined in :samp:`gcc-dg.exp`.

Most uses of torture options can override the default lists by defining
:samp:`{TORTURE_OPTIONS}` or add to the default list by defining
:samp:`{ADDITIONAL_TORTURE_OPTIONS}`.  Define these in a :samp:`.dejagnurc`
file or add them to the :samp:`site.exp` file; for example

.. code-block:: c++

  set ADDITIONAL_TORTURE_OPTIONS  [list \
    { -O2 -ftree-loop-linear } \
    { -O2 -fpeel-loops } ]
