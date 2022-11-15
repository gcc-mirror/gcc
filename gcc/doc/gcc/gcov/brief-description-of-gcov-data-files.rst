..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gcov-data-files:

Brief Description of gcov Data Files
************************************

:command:`gcov` uses two files for profiling.  The names of these files
are derived from the original *object* file by substituting the
file suffix with either :samp:`.gcno`, or :samp:`.gcda`.  The files
contain coverage and profile data stored in a platform-independent format.
The :samp:`.gcno` files are placed in the same directory as the object
file.  By default, the :samp:`.gcda` files are also stored in the same
directory as the object file, but the GCC :option:`-fprofile-dir` option
may be used to store the :samp:`.gcda` files in a separate directory.

The :samp:`.gcno` notes file is generated when the source file is compiled
with the GCC :option:`-ftest-coverage` option.  It contains information to
reconstruct the basic block graphs and assign source line numbers to
blocks.

The :samp:`.gcda` count data file is generated when a program containing
object files built with the GCC :option:`-fprofile-arcs` option is executed.
A separate :samp:`.gcda` file is created for each object file compiled with
this option.  It contains arc transition counts, value profile counts, and
some summary information.

It is not recommended to access the coverage files directly.
Consumers should use the intermediate format that is provided
by :command:`gcov` tool via :option:`--json-format` option.
