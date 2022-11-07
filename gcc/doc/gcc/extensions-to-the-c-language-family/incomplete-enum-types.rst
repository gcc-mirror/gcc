..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _incomplete-enums:

Incomplete enum Types
*********************

You can define an ``enum`` tag without specifying its possible values.
This results in an incomplete type, much like what you get if you write
``struct foo`` without describing the elements.  A later declaration
that does specify the possible values completes the type.

You cannot allocate variables or storage using the type while it is
incomplete.  However, you can work with pointers to that type.

This extension may not be very useful, but it makes the handling of
``enum`` more consistent with the way ``struct`` and ``union``
are handled.

This extension is not supported by GNU C++.