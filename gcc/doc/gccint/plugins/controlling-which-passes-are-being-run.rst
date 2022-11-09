..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _plugins-gate:

Controlling which passes are being run
**************************************

After the original gate function for a pass is called, its result
- the gate status - is stored as an integer.
Then the event ``PLUGIN_OVERRIDE_GATE`` is invoked, with a pointer
to the gate status in the ``gcc_data`` parameter to the callback function.
A nonzero value of the gate status means that the pass is to be executed.
You can both read and write the gate status via the passed pointer.
