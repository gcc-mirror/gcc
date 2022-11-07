..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _conditionally-supported-behavior:

Conditionally-Supported Behavior
********************************

Each implementation shall include documentation that identifies
all conditionally-supported constructs that it does not support (C++0x
1.4).

* Whether an argument of class type with a non-trivial copy
  constructor or destructor can be passed to ... (C++0x 5.2.2).

  Such argument passing is supported, using the same
  pass-by-invisible-reference approach used for normal function
  arguments of such types.