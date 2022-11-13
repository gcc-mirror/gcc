..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: additional floating types, _Floatn data types, _Floatnx data types, __float80 data type, __float128 data type, __ibm128 data type, w floating point suffix, q floating point suffix, W floating point suffix, Q floating point suffix

.. _floating-types:

Additional Floating Types
*************************

ISO/IEC TS 18661-3:2015 defines C support for additional floating
types ``_Floatn`` and ``_Floatnx``, and GCC supports
these type names; the set of types supported depends on the target
architecture.  These types are not supported when compiling C++.
Constants with these types use suffixes ``fn`` or
``Fn`` and ``fnx`` or ``Fnx``.  These type
names can be used together with ``_Complex`` to declare complex
types.

As an extension, GNU C and GNU C++ support additional floating
types, which are not supported by all targets.

* ``__float128`` is available on i386, x86_64, IA-64, and
  hppa HP-UX, as well as on PowerPC GNU/Linux targets that enable
  the vector scalar (VSX) instruction set.  ``__float128`` supports
  the 128-bit floating type.  On i386, x86_64, PowerPC, and IA-64
  other than HP-UX, ``__float128`` is an alias for ``_Float128``.
  On hppa and IA-64 HP-UX, ``__float128`` is an alias for ``long
  double``.

* ``__float80`` is available on the i386, x86_64, and IA-64
  targets, and supports the 80-bit (``XFmode``) floating type.  It is
  an alias for the type name ``_Float64x`` on these targets.

* ``__ibm128`` is available on PowerPC targets, and provides
  access to the IBM extended double format which is the current format
  used for ``long double``.  When ``long double`` transitions to
  ``__float128`` on PowerPC in the future, ``__ibm128`` will remain
  for use in conversions between the two types.

Support for these additional types includes the arithmetic operators:
add, subtract, multiply, divide; unary arithmetic operators;
relational operators; equality operators; and conversions to and from
integer and other floating types.  Use a suffix :samp:`w` or :samp:`W`
in a literal constant of type ``__float80`` or type
``__ibm128``.  Use a suffix :samp:`q` or :samp:`Q` for ``__float128``.

In order to use ``_Float128``, ``__float128``, and ``__ibm128``
on PowerPC Linux systems, you must use the :option:`-mfloat128` option. It is
expected in future versions of GCC that ``_Float128`` and ``__float128``
will be enabled automatically.

The ``_Float128`` type is supported on all systems where
``__float128`` is supported or where ``long double`` has the
IEEE binary128 format.  The ``_Float64x`` type is supported on all
systems where ``__float128`` is supported.  The ``_Float32``
type is supported on all systems supporting IEEE binary32; the
``_Float64`` and ``_Float32x`` types are supported on all systems
supporting IEEE binary64.  The ``_Float16`` type is supported on AArch64
systems by default, on ARM systems when the IEEE format for 16-bit
floating-point types is selected with :option:`-mfp16-format=ieee` and,
for both C and C++, on x86 systems with SSE2 enabled. GCC does not currently
support ``_Float128x`` on any systems.

On the i386, x86_64, IA-64, and HP-UX targets, you can declare complex
types using the corresponding internal complex type, ``XCmode`` for
``__float80`` type and ``TCmode`` for ``__float128`` type:

.. code-block:: c++

  typedef _Complex float __attribute__((mode(TC))) _Complex128;
  typedef _Complex float __attribute__((mode(XC))) _Complex80;

On the PowerPC Linux VSX targets, you can declare complex types using
the corresponding internal complex type, ``KCmode`` for
``__float128`` type and ``ICmode`` for ``__ibm128`` type:

.. code-block:: c++

  typedef _Complex float __attribute__((mode(KC))) _Complex_float128;
  typedef _Complex float __attribute__((mode(IC))) _Complex_ibm128;