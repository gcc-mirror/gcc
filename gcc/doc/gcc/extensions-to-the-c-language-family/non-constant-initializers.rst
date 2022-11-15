..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: initializers, non-constant, non-constant initializers

.. _initializers:

Non-Constant Initializers
*************************

As in standard C++ and ISO C99, the elements of an aggregate initializer for an
automatic variable are not required to be constant expressions in GNU C.
Here is an example of an initializer with run-time varying elements:

.. code-block:: c++

  foo (float f, float g)
  {
    float beat_freqs[2] = { f-g, f+g };
    /* ... */
  }
