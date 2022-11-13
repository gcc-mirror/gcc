..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gcov-testing:

Support for testing gcov
************************

Language-independent support for testing :command:`gcov`, and for checking
that branch profiling produces expected values, is provided by the
expect file :samp:`lib/gcov.exp`.  :command:`gcov` tests also rely on procedures
in :samp:`lib/gcc-dg.exp` to compile and run the test program.  A typical
:command:`gcov` test contains the following DejaGnu commands within comments:

.. code-block:: c++

  { dg-options "--coverage" }
  { dg-do run { target native } }
  { dg-final { run-gcov sourcefile } }

Checks of :command:`gcov` output can include line counts, branch percentages,
and call return percentages.  All of these checks are requested via
commands that appear in comments in the test's source file.
Commands to check line counts are processed by default.
Commands to check branch percentages and call return percentages are
processed if the :command:`run-gcov` command has arguments ``branches``
or ``calls``, respectively.  For example, the following specifies
checking both, as well as passing :option:`-b` to :command:`gcov`:

.. code-block:: c++

  { dg-final { run-gcov branches calls { -b sourcefile } } }

A line count command appears within a comment on the source line
that is expected to get the specified count and has the form
``count(cnt)``.  A test should only check line counts for
lines that will get the same count for any architecture.

Commands to check branch percentages (``branch``) and call
return percentages (``returns``) are very similar to each other.
A beginning command appears on or before the first of a range of
lines that will report the percentage, and the ending command
follows that range of lines.  The beginning command can include a
list of percentages, all of which are expected to be found within
the range.  A range is terminated by the next command of the same
kind.  A command ``branch(end)`` or ``returns(end)`` marks
the end of a range without starting a new one.  For example:

.. code-block:: c++

  if (i > 10 && j > i && j < 20)  /* branch(27 50 75) */
                                  /* branch(end) */
    foo (i, j);

For a call return percentage, the value specified is the
percentage of calls reported to return.  For a branch percentage,
the value is either the expected percentage or 100 minus that
value, since the direction of a branch can differ depending on the
target or the optimization level.

Not all branches and calls need to be checked.  A test should not
check for branches that might be optimized away or replaced with
predicated instructions.  Don't check for calls inserted by the
compiler or ones that might be inlined or optimized away.

A single test can check for combinations of line counts, branch
percentages, and call return percentages.  The command to check a
line count must appear on the line that will report that count, but
commands to check branch percentages and call return percentages can
bracket the lines that report them.