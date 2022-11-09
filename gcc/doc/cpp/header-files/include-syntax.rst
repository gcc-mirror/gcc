..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: #include

.. _include-syntax:

Include Syntax
**************

Both user and system header files are included using the preprocessing
directive :samp:`#include`.  It has two variants:

:samp:`#include <{file}>`
  This variant is used for system header files.  It searches for a file
  named :samp:`{file}` in a standard list of system directories.  You can prepend
  directories to this list with the :option:`-I` option (see :ref:`invocation`).

:samp:`#include "{file}"`
  This variant is used for header files of your own program.  It
  searches for a file named :samp:`{file}` first in the directory containing
  the current file, then in the quote directories and then the same
  directories used for ``<file>``.  You can prepend directories
  to the list of quote directories with the :option:`-iquote` option.

The argument of :samp:`#include`, whether delimited with quote marks or
angle brackets, behaves like a string constant in that comments are not
recognized, and macro names are not expanded.  Thus, ``#include
<x/*y>`` specifies inclusion of a system header file named :samp:`x/*y`.

However, if backslashes occur within :samp:`{file}`, they are considered
ordinary text characters, not escape characters.  None of the character
escape sequences appropriate to string constants in C are processed.
Thus, ``#include "x\n\\y"`` specifies a filename containing three
backslashes.  (Some systems interpret :samp:`\\` as a pathname separator.
All of these also interpret :samp:`/` the same way.  It is most portable
to use only :samp:`/`.)

It is an error if there is anything (other than comments) on the line
after the file name.
