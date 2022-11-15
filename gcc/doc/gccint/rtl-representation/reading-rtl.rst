..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _reading-rtl:

Reading RTL
***********

To read an RTL object from a file, call ``read_rtx``.  It takes one
argument, a stdio stream, and returns a single RTL object.  This routine
is defined in :samp:`read-rtl.cc`.  It is not available in the compiler
itself, only the various programs that generate the compiler back end
from the machine description.

People frequently have the idea of using RTL stored as text in a file as
an interface between a language front end and the bulk of GCC.  This
idea is not feasible.

GCC was designed to use RTL internally only.  Correct RTL for a given
program is very dependent on the particular target machine.  And the RTL
does not contain all the information about the program.

The proper way to interface GCC to a new language front end is with
the 'tree' data structure, described in the files :samp:`tree.h` and
:samp:`tree.def`.  The documentation for this structure (see :ref:`generic`)
is incomplete.
