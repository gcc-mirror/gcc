..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gfortran_list_separator:

GFORTRAN_LIST_SEPARATOR---Separator for list output
***************************************************

This environment variable specifies the separator when writing
list-directed output.  It may contain any number of spaces and
at most one comma.  If you specify this on the command line,
be sure to quote spaces, as in

.. code-block:: shell-session

  $ GFORTRAN_LIST_SEPARATOR='  ,  ' ./a.out

when :command:`a.out` is the compiled Fortran program that you want to run.
Default is a single space.
