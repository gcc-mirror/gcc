..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: complex numbers, _Complex keyword, __complex__ keyword

.. _complex:

Complex Numbers
***************

ISO C99 supports complex floating data types, and as an extension GCC
supports them in C90 mode and in C++.  GCC also supports complex integer data
types which are not part of ISO C99.  You can declare complex types
using the keyword ``_Complex``.  As an extension, the older GNU
keyword ``__complex__`` is also supported.

For example, :samp:`_Complex double x;` declares ``x`` as a
variable whose real part and imaginary part are both of type
``double``.  :samp:`_Complex short int y;` declares ``y`` to
have real and imaginary parts of type ``short int`` ; this is not
likely to be useful, but it shows that the set of complex types is
complete.

To write a constant with a complex data type, use the suffix :samp:`i` or
:samp:`j` (either one; they are equivalent).  For example, ``2.5fi``
has type ``_Complex float`` and ``3i`` has type
``_Complex int``.  Such a constant always has a pure imaginary
value, but you can form any complex value you like by adding one to a
real constant.  This is a GNU extension; if you have an ISO C99
conforming C library (such as the GNU C Library), and want to construct complex
constants of floating type, you should include ``<complex.h>`` and
use the macros ``I`` or ``_Complex_I`` instead.

The ISO C++14 library also defines the :samp:`i` suffix, so C++14 code
that includes the :samp:`<complex>` header cannot use :samp:`i` for the
GNU extension.  The :samp:`j` suffix still has the GNU meaning.

GCC can handle both implicit and explicit casts between the ``_Complex``
types and other ``_Complex`` types as casting both the real and imaginary
parts to the scalar type.
GCC can handle implicit and explicit casts from a scalar type to a ``_Complex``
type and where the imaginary part will be considered zero.
The C front-end can handle implicit and explicit casts from a ``_Complex`` type
to a scalar type where the imaginary part will be ignored. In C++ code, this cast
is considered illformed and G++ will error out.

GCC provides a built-in function ``__builtin_complex`` will can be used to
construct a complex value.

.. index:: __real__ keyword, __imag__ keyword

GCC has a few extensions which can be used to extract the real
and the imaginary part of the complex-valued expression. Note
these expressions are lvalues if the :samp:`{exp}` is an lvalue.
These expressions operands have the type of a complex type
which might get prompoted to a complex type from a scalar type.
E.g. ``__real__ (int)x`` is the same as casting to
``_Complex int`` before ``__real__`` is done.

.. list-table::
   :header-rows: 1

   * - Expression
     - Description

   * - ``__real__ exp``
     - Extract the real part of :samp:`{exp}`.
   * - ``__imag__ exp``
     - Extract the imaginary part of :samp:`{exp}`.

For values of floating point, you should use the ISO C99
functions, declared in ``<complex.h>`` and also provided as
built-in functions by GCC.

.. list-table::
   :header-rows: 1

   * - Expression
     - float
     - double
     - long double

   * - ``__real__ exp``
     - ``crealf``
     - ``creal``
     - ``creall``
   * - ``__imag__ exp``
     - ``cimagf``
     - ``cimag``
     - ``cimagl``

.. index:: complex conjugation

The operator :samp:`~` performs complex conjugation when used on a value
with a complex type.  This is a GNU extension; for values of
floating type, you should use the ISO C99 functions ``conjf``,
``conj`` and ``conjl``, declared in ``<complex.h>`` and also
provided as built-in functions by GCC. Note unlike the ``__real__``
and ``__imag__`` operators, this operator will not do an implicit cast
to the complex type because the :samp:`~` is already a normal operator.

GCC can allocate complex automatic variables in a noncontiguous
fashion; it's even possible for the real part to be in a register while
the imaginary part is on the stack (or vice versa).  Only the DWARF
debug info format can represent this, so use of DWARF is recommended.
If you are using the stabs debug info format, GCC describes a noncontiguous
complex variable as if it were two separate variables of noncomplex type.
If the variable's actual name is ``foo``, the two fictitious
variables are named ``foo$real`` and ``foo$imag``.  You can
examine and set these two fictitious variables with your debugger.

.. function:: type __builtin_complex (real, imag)

  The built-in function ``__builtin_complex`` is provided for use in
  implementing the ISO C11 macros ``CMPLXF``, ``CMPLX`` and
  ``CMPLXL``.  :samp:`{real}` and :samp:`{imag}` must have the same type, a
  real binary floating-point type, and the result has the corresponding
  complex type with real and imaginary parts :samp:`{real}` and :samp:`{imag}`.
  Unlike :samp:`{real} + I * {imag}`, this works even when
  infinities, NaNs and negative zeros are involved.