..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _ti-c6x-built-in-functions:

TI C6X Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides intrinsics to access certain instructions of the TI C6X
processors.  These intrinsics, listed below, are available after
inclusion of the ``c6x_intrinsics.h`` header file.  They map directly
to C6X instructions.

.. code-block:: c++

  int _sadd (int, int);
  int _ssub (int, int);
  int _sadd2 (int, int);
  int _ssub2 (int, int);
  long long _mpy2 (int, int);
  long long _smpy2 (int, int);
  int _add4 (int, int);
  int _sub4 (int, int);
  int _saddu4 (int, int);

  int _smpy (int, int);
  int _smpyh (int, int);
  int _smpyhl (int, int);
  int _smpylh (int, int);

  int _sshl (int, int);
  int _subc (int, int);

  int _avg2 (int, int);
  int _avgu4 (int, int);

  int _clrr (int, int);
  int _extr (int, int);
  int _extru (int, int);
  int _abs (int);
  int _abs2 (int);