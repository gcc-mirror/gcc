..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: fixed-point types, _Fract data type, _Accum data type, _Sat data type, hr fixed-suffix, r fixed-suffix, lr fixed-suffix, llr fixed-suffix, uhr fixed-suffix, ur fixed-suffix, ulr fixed-suffix, ullr fixed-suffix, hk fixed-suffix, k fixed-suffix, lk fixed-suffix, llk fixed-suffix, uhk fixed-suffix, uk fixed-suffix, ulk fixed-suffix, ullk fixed-suffix, HR fixed-suffix, R fixed-suffix, LR fixed-suffix, LLR fixed-suffix, UHR fixed-suffix, UR fixed-suffix, ULR fixed-suffix, ULLR fixed-suffix, HK fixed-suffix, K fixed-suffix, LK fixed-suffix, LLK fixed-suffix, UHK fixed-suffix, UK fixed-suffix, ULK fixed-suffix, ULLK fixed-suffix

.. _fixed-point:

Fixed-Point Types
*****************

As an extension, GNU C supports fixed-point types as
defined in the N1169 draft of ISO/IEC DTR 18037.  Support for fixed-point
types in GCC will evolve as the draft technical report changes.
Calling conventions for any target might also change.  Not all targets
support fixed-point types.

The fixed-point types are
``short _Fract``,
``_Fract``,
``long _Fract``,
``long long _Fract``,
``unsigned short _Fract``,
``unsigned _Fract``,
``unsigned long _Fract``,
``unsigned long long _Fract``,
``_Sat short _Fract``,
``_Sat _Fract``,
``_Sat long _Fract``,
``_Sat long long _Fract``,
``_Sat unsigned short _Fract``,
``_Sat unsigned _Fract``,
``_Sat unsigned long _Fract``,
``_Sat unsigned long long _Fract``,
``short _Accum``,
``_Accum``,
``long _Accum``,
``long long _Accum``,
``unsigned short _Accum``,
``unsigned _Accum``,
``unsigned long _Accum``,
``unsigned long long _Accum``,
``_Sat short _Accum``,
``_Sat _Accum``,
``_Sat long _Accum``,
``_Sat long long _Accum``,
``_Sat unsigned short _Accum``,
``_Sat unsigned _Accum``,
``_Sat unsigned long _Accum``,
``_Sat unsigned long long _Accum``.

Fixed-point data values contain fractional and optional integral parts.
The format of fixed-point data varies and depends on the target machine.

Support for fixed-point types includes:

* prefix and postfix increment and decrement operators (``++``, ``--``)

* unary arithmetic operators (``+``, ``-``, ``!``)

* binary arithmetic operators (``+``, ``-``, ``*``, ``/``)

* binary shift operators (``<<``, ``>>``)

* relational operators (``<``, ``<=``, ``>=``, ``>``)

* equality operators (``==``, ``!=``)

* assignment operators (``+=``, ``-=``, ``*=``, ``/=``,
  ``<<=``, ``>>=``)

* conversions to and from integer, floating-point, or fixed-point types

Use a suffix in a fixed-point literal constant:

* :samp:`hr` or :samp:`HR` for ``short _Fract`` and
  ``_Sat short _Fract``

* :samp:`r` or :samp:`R` for ``_Fract`` and ``_Sat _Fract``

* :samp:`lr` or :samp:`LR` for ``long _Fract`` and
  ``_Sat long _Fract``

* :samp:`llr` or :samp:`LLR` for ``long long _Fract`` and
  ``_Sat long long _Fract``

* :samp:`uhr` or :samp:`UHR` for ``unsigned short _Fract`` and
  ``_Sat unsigned short _Fract``

* :samp:`ur` or :samp:`UR` for ``unsigned _Fract`` and
  ``_Sat unsigned _Fract``

* :samp:`ulr` or :samp:`ULR` for ``unsigned long _Fract`` and
  ``_Sat unsigned long _Fract``

* :samp:`ullr` or :samp:`ULLR` for ``unsigned long long _Fract``
  and ``_Sat unsigned long long _Fract``

* :samp:`hk` or :samp:`HK` for ``short _Accum`` and
  ``_Sat short _Accum``

* :samp:`k` or :samp:`K` for ``_Accum`` and ``_Sat _Accum``

* :samp:`lk` or :samp:`LK` for ``long _Accum`` and
  ``_Sat long _Accum``

* :samp:`llk` or :samp:`LLK` for ``long long _Accum`` and
  ``_Sat long long _Accum``

* :samp:`uhk` or :samp:`UHK` for ``unsigned short _Accum`` and
  ``_Sat unsigned short _Accum``

* :samp:`uk` or :samp:`UK` for ``unsigned _Accum`` and
  ``_Sat unsigned _Accum``

* :samp:`ulk` or :samp:`ULK` for ``unsigned long _Accum`` and
  ``_Sat unsigned long _Accum``

* :samp:`ullk` or :samp:`ULLK` for ``unsigned long long _Accum``
  and ``_Sat unsigned long long _Accum``

GCC support of fixed-point types as specified by the draft technical report
is incomplete:

* Pragmas to control overflow and rounding behaviors are not implemented.

Fixed-point types are supported by the DWARF debug information format.
