..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: pointers to arrays, const qualifier

.. _pointers-to-arrays:

Pointers to Arrays with Qualifiers Work as Expected
***************************************************

In GNU C, pointers to arrays with qualifiers work similar to pointers
to other qualified types. For example, a value of type ``int (*)[5]``
can be used to initialize a variable of type ``const int (*)[5]``.
These types are incompatible in ISO C because the ``const`` qualifier
is formally attached to the element type of the array and not the
array itself.

.. code-block:: c++

  extern void
  transpose (int N, int M, double out[M][N], const double in[N][M]);
  double x[3][2];
  double y[2][3];
  ...
  transpose(3, 2, y, x);