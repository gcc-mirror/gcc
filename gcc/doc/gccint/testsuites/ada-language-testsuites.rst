..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _ada-tests:

Ada Language Testsuites
***********************

The Ada testsuite includes executable tests from the ACATS
testsuite, publicly available at
http://www.ada-auth.org/acats.html.

These tests are integrated in the GCC testsuite in the
:samp:`ada/acats` directory, and
enabled automatically when running ``make check``, assuming
the Ada language has been enabled when configuring GCC.

You can also run the Ada testsuite independently, using
``make check-ada``, or run a subset of the tests by specifying which
chapter to run, e.g.:

.. code-block:: c++

  $ make check-ada CHAPTERS="c3 c9"

The tests are organized by directory, each directory corresponding to
a chapter of the Ada Reference Manual.  So for example, :samp:`c9` corresponds
to chapter 9, which deals with tasking features of the language.

The tests are run using two :command:`sh` scripts: :samp:`run_acats` and
:samp:`run_all.sh`.  To run the tests using a simulator or a cross
target, see the small
customization section at the top of :samp:`run_all.sh`.

These tests are run using the build tree: they can be run without doing
a ``make install``.
