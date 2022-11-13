..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: hex floats

.. _hex-floats:

Hex Floats
**********

ISO C99 and ISO C++17 support floating-point numbers written not only in
the usual decimal notation, such as ``1.55e1``, but also numbers such as
``0x1.fp3`` written in hexadecimal format.  As a GNU extension, GCC
supports this in C90 mode (except in some cases when strictly
conforming) and in C++98, C++11 and C++14 modes.  In that format the
:samp:`0x` hex introducer and the :samp:`p` or :samp:`P` exponent field are
mandatory.  The exponent is a decimal number that indicates the power of
2 by which the significant part is multiplied.  Thus :samp:`0x1.f` is

1 15/16,
:samp:`p3` multiplies it by 8, and the value of ``0x1.fp3``
is the same as ``1.55e1``.

Unlike for floating-point numbers in the decimal notation the exponent
is always required in the hexadecimal notation.  Otherwise the compiler
would not be able to resolve the ambiguity of, e.g., ``0x1.f``.  This
could mean ``1.0f`` or ``1.9375`` since :samp:`f` is also the
extension for floating-point constants of type ``float``.