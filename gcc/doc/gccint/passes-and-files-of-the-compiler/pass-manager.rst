..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _pass-manager:

Pass manager
************

The pass manager is located in :samp:`passes.cc`, :samp:`tree-optimize.c`
and :samp:`tree-pass.h`.
It processes passes as described in :samp:`passes.def`.
Its job is to run all of the individual passes in the correct order,
and take care of standard bookkeeping that applies to every pass.

The theory of operation is that each pass defines a structure that
represents everything we need to know about that pass---when it
should be run, how it should be run, what intermediate language
form or on-the-side data structures it needs.  We register the pass
to be run in some particular order, and the pass manager arranges
for everything to happen in the correct order.

The actuality doesn't completely live up to the theory at present.
Command-line switches and ``timevar_id_t`` enumerations must still
be defined elsewhere.  The pass manager validates constraints but does
not attempt to (re-)generate data structures or lower intermediate
language form based on the requirements of the next pass.  Nevertheless,
what is present is useful, and a far sight better than nothing at all.

Each pass should have a unique name.
Each pass may have its own dump file (for GCC debugging purposes).
Passes with a name starting with a star do not dump anything.
Sometimes passes are supposed to share a dump file / option name.
To still give these unique names, you can use a prefix that is delimited
by a space from the part that is used for the dump file / option name.
E.g. When the pass name is "ud dce", the name used for dump file/options
is "dce".

.. todo:: describe the global variables set up by the pass manager,
  and a brief description of how a new pass should use it.
  I need to look at what info RTL passes use first…