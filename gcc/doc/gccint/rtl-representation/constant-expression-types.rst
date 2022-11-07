..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RTL constants, RTL constant expression types

.. _constants:

Constant Expression Types
*************************

The simplest RTL expressions are those that represent constant values.

.. index:: const_int

:samp:`(const_int {i})`
  This type of expression represents the integer value :samp:`{i}`.  :samp:`{i}`
  is customarily accessed with the macro ``INTVAL`` as in
  ``INTVAL (exp)``, which is equivalent to ``XWINT (exp, 0)``.

  Constants generated for modes with fewer bits than in
  ``HOST_WIDE_INT`` must be sign extended to full width (e.g., with
  ``gen_int_mode``).  For constants for modes with more bits than in
  ``HOST_WIDE_INT`` the implied high order bits of that constant are
  copies of the top bit.  Note however that values are neither
  inherently signed nor inherently unsigned; where necessary, signedness
  is determined by the rtl operation instead.

  .. index:: const0_rtx, const1_rtx, const2_rtx, constm1_rtx

  There is only one expression object for the integer value zero; it is
  the value of the variable ``const0_rtx``.  Likewise, the only
  expression for integer value one is found in ``const1_rtx``, the only
  expression for integer value two is found in ``const2_rtx``, and the
  only expression for integer value negative one is found in
  ``constm1_rtx``.  Any attempt to create an expression of code
  ``const_int`` and value zero, one, two or negative one will return
  ``const0_rtx``, ``const1_rtx``, ``const2_rtx`` or
  ``constm1_rtx`` as appropriate.

  .. index:: const_true_rtx

  Similarly, there is only one object for the integer whose value is
  ``STORE_FLAG_VALUE``.  It is found in ``const_true_rtx``.  If
  ``STORE_FLAG_VALUE`` is one, ``const_true_rtx`` and
  ``const1_rtx`` will point to the same object.  If
  ``STORE_FLAG_VALUE`` is -1, ``const_true_rtx`` and
  ``constm1_rtx`` will point to the same object.

  .. index:: const_double

:samp:`(const_double:{m} {i0} {i1} ...)`
  This represents either a floating-point constant of mode :samp:`{m}` or
  (on older ports that do not define
  ``TARGET_SUPPORTS_WIDE_INT``) an integer constant too large to fit
  into ``HOST_BITS_PER_WIDE_INT`` bits but small enough to fit within
  twice that number of bits.  In the latter case, :samp:`{m}` will be
  ``VOIDmode``.  For integral values constants for modes with more
  bits than twice the number in ``HOST_WIDE_INT`` the implied high
  order bits of that constant are copies of the top bit of
  ``CONST_DOUBLE_HIGH``.  Note however that integral values are
  neither inherently signed nor inherently unsigned; where necessary,
  signedness is determined by the rtl operation instead.

  On more modern ports, ``CONST_DOUBLE`` only represents floating
  point values.  New ports define ``TARGET_SUPPORTS_WIDE_INT`` to
  make this designation.

  .. index:: CONST_DOUBLE_LOW

  If :samp:`{m}` is ``VOIDmode``, the bits of the value are stored in
  :samp:`{i0}` and :samp:`{i1}`.  :samp:`{i0}` is customarily accessed with the macro
  ``CONST_DOUBLE_LOW`` and :samp:`{i1}` with ``CONST_DOUBLE_HIGH``.

  If the constant is floating point (regardless of its precision), then
  the number of integers used to store the value depends on the size of
  ``REAL_VALUE_TYPE`` (see :ref:`floating-point`).  The integers
  represent a floating point number, but not precisely in the target
  machine's or host machine's floating point format.  To convert them to
  the precise bit pattern used by the target machine, use the macro
  ``REAL_VALUE_TO_TARGET_DOUBLE`` and friends (see :ref:`data-output`).

  .. index:: const_double_zero

  The host dependency for the number of integers used to store a double
  value makes it problematic for machine descriptions to use expressions
  of code ``const_double`` and therefore a syntactic alias has been
  provided:

  .. code-block:: c++

    (const_double_zero:m)

  standing for:

  .. code-block:: c++

    (const_double:m 0 0 ...)

  for matching the floating-point value zero, possibly the only useful one.

  .. index:: CONST_WIDE_INT

:samp:`(const_wide_int:{m} {nunits} {elt0} ...)`
  This contains an array of ``HOST_WIDE_INT`` s that is large enough
  to hold any constant that can be represented on the target.  This form
  of rtl is only used on targets that define
  ``TARGET_SUPPORTS_WIDE_INT`` to be nonzero and then
  ``CONST_DOUBLE`` s are only used to hold floating-point values.  If
  the target leaves ``TARGET_SUPPORTS_WIDE_INT`` defined as 0,
  ``CONST_WIDE_INT`` s are not used and ``CONST_DOUBLE`` s are as
  they were before.

  The values are stored in a compressed format.  The higher-order
  0s or -1s are not represented if they are just the logical sign
  extension of the number that is represented.

  .. index:: CONST_WIDE_INT_VEC

:samp:`CONST_WIDE_INT_VEC ({code})`
  Returns the entire array of ``HOST_WIDE_INT`` s that are used to
  store the value.  This macro should be rarely used.

  .. index:: CONST_WIDE_INT_NUNITS

:samp:`CONST_WIDE_INT_NUNITS ({code})`
  The number of ``HOST_WIDE_INT`` s used to represent the number.
  Note that this generally is smaller than the number of
  ``HOST_WIDE_INT`` s implied by the mode size.

  .. index:: CONST_WIDE_INT_ELT

:samp:`CONST_WIDE_INT_ELT ({code},{i})`
  Returns the ``i`` th element of the array.   Element 0 is contains
  the low order bits of the constant.

  .. index:: const_fixed

:samp:`(const_fixed:{m} ...)`
  Represents a fixed-point constant of mode :samp:`{m}`.
  The operand is a data structure of type ``struct fixed_value`` and
  is accessed with the macro ``CONST_FIXED_VALUE``.  The high part of
  data is accessed with ``CONST_FIXED_VALUE_HIGH`` ; the low part is
  accessed with ``CONST_FIXED_VALUE_LOW``.

  .. index:: const_poly_int

:samp:`(const_poly_int:{m} [{c0} {c1} ...])`
  Represents a ``poly_int`` -style polynomial integer with coefficients
  :samp:`{c0}`, :samp:`{c1}`, ....  The coefficients are ``wide_int`` -based
  integers rather than rtxes.  ``CONST_POLY_INT_COEFFS`` gives the
  values of individual coefficients (which is mostly only useful in
  low-level routines) and ``const_poly_int_value`` gives the full
  ``poly_int`` value.

  .. index:: const_vector

:samp:`(const_vector:{m} [{x0} {x1} ...])`
  Represents a vector constant.  The values in square brackets are
  elements of the vector, which are always ``const_int``,
  ``const_wide_int``, ``const_double`` or ``const_fixed``
  expressions.

  Each vector constant :samp:`{v}` is treated as a specific instance of an
  arbitrary-length sequence that itself contains
  :samp:`CONST_VECTOR_NPATTERNS ({v})` interleaved patterns.  Each
  pattern has the form:

  .. code-block:: c++

    { base0, base1, base1 + step, base1 + step * 2, ... }

  The first three elements in each pattern are enough to determine the
  values of the other elements.  However, if all :samp:`{step}` s are zero,
  only the first two elements are needed.  If in addition each :samp:`{base1}`
  is equal to the corresponding :samp:`{base0}`, only the first element in
  each pattern is needed.  The number of determining elements per pattern
  is given by :samp:`CONST_VECTOR_NELTS_PER_PATTERN ({v})`.

  For example, the constant:

  .. code-block:: c++

    { 0, 1, 2, 6, 3, 8, 4, 10, 5, 12, 6, 14, 7, 16, 8, 18 }

  is interpreted as an interleaving of the sequences:

  .. code-block:: c++

    { 0, 2, 3, 4, 5, 6, 7, 8 }
    { 1, 6, 8, 10, 12, 14, 16, 18 }

  where the sequences are represented by the following patterns:

  .. code-block:: c++

    base0 == 0, base1 == 2, step == 1
    base0 == 1, base1 == 6, step == 2

  In this case:

  .. code-block:: c++

    CONST_VECTOR_NPATTERNS (v) == 2
    CONST_VECTOR_NELTS_PER_PATTERN (v) == 3

  Thus the first 6 elements (:samp:`{ 0, 1, 2, 6, 3, 8 }`) are enough
  to determine the whole sequence; we refer to them as the 'encoded'
  elements.  They are the only elements present in the square brackets
  for variable-length ``const_vector`` s (i.e. for
  ``const_vector`` s whose mode :samp:`{m}` has a variable number of
  elements).  However, as a convenience to code that needs to handle
  both ``const_vector`` s and ``parallel`` s, all elements are
  present in the square brackets for fixed-length ``const_vector`` s;
  the encoding scheme simply reduces the amount of work involved in
  processing constants that follow a regular pattern.

  Sometimes this scheme can create two possible encodings of the same
  vector.  For example { 0, 1 } could be seen as two patterns with
  one element each or one pattern with two elements (:samp:`{base0}` and
  :samp:`{base1}`).  The canonical encoding is always the one with the
  fewest patterns or (if both encodings have the same number of
  petterns) the one with the fewest encoded elements.

  :samp:`const_vector_encoding_nelts ({v})` gives the total number of
  encoded elements in :samp:`{v}`, which is 6 in the example above.
  ``CONST_VECTOR_ENCODED_ELT (v, i)`` accesses the value
  of encoded element :samp:`{i}`.

  :samp:`CONST_VECTOR_DUPLICATE_P ({v})` is true if :samp:`{v}` simply contains
  repeated instances of :samp:`CONST_VECTOR_NPATTERNS ({v})` values.  This is
  a shorthand for testing :samp:`CONST_VECTOR_NELTS_PER_PATTERN ({v}) == 1`.

  :samp:`CONST_VECTOR_STEPPED_P ({v})` is true if at least one
  pattern in :samp:`{v}` has a nonzero step.  This is a shorthand for
  testing :samp:`CONST_VECTOR_NELTS_PER_PATTERN ({v}) == 3`.

  ``CONST_VECTOR_NUNITS (v)`` gives the total number of elements
  in :samp:`{v}` ; it is a shorthand for getting the number of units in
  :samp:`GET_MODE ({v})`.

  The utility function ``const_vector_elt`` gives the value of an
  arbitrary element as an ``rtx``.  ``const_vector_int_elt`` gives
  the same value as a ``wide_int``.

  .. index:: const_string

:samp:`(const_string {str})`
  Represents a constant string with value :samp:`{str}`.  Currently this is
  used only for insn attributes (see :ref:`insn-attributes`) since constant
  strings in C are placed in memory.

  .. index:: symbol_ref

:samp:`(symbol_ref:{mode} {symbol})`
  Represents the value of an assembler label for data.  :samp:`{symbol}` is
  a string that describes the name of the assembler label.  If it starts
  with a :samp:`*`, the label is the rest of :samp:`{symbol}` not including
  the :samp:`*`.  Otherwise, the label is :samp:`{symbol}`, usually prefixed
  with :samp:`_`.

  The ``symbol_ref`` contains a mode, which is usually ``Pmode``.
  Usually that is the only mode for which a symbol is directly valid.

  .. index:: label_ref

:samp:`(label_ref:{mode} {label})`
  Represents the value of an assembler label for code.  It contains one
  operand, an expression, which must be a ``code_label`` or a ``note``
  of type ``NOTE_INSN_DELETED_LABEL`` that appears in the instruction
  sequence to identify the place where the label should go.

  The reason for using a distinct expression type for code label
  references is so that jump optimization can distinguish them.

  The ``label_ref`` contains a mode, which is usually ``Pmode``.
  Usually that is the only mode for which a label is directly valid.

  .. index:: const

:samp:`(const:{m} {exp})`
  Represents a constant that is the result of an assembly-time
  arithmetic computation.  The operand, :samp:`{exp}`, contains only
  ``const_int``, ``symbol_ref``, ``label_ref`` or ``unspec``
  expressions, combined with ``plus`` and ``minus``.  Any such
  ``unspec`` s are target-specific and typically represent some form
  of relocation operator.  :samp:`{m}` should be a valid address mode.

  .. index:: high

:samp:`(high:{m} {exp})`
  Represents the high-order bits of :samp:`{exp}`.
  The number of bits is machine-dependent and is
  normally the number of bits specified in an instruction that initializes
  the high order bits of a register.  It is used with ``lo_sum`` to
  represent the typical two-instruction sequence used in RISC machines to
  reference large immediate values and/or link-time constants such
  as global memory addresses.  In the latter case, :samp:`{m}` is ``Pmode``
  and :samp:`{exp}` is usually a constant expression involving ``symbol_ref``.

.. index:: CONST0_RTX, CONST1_RTX, CONST2_RTX

The macro ``CONST0_RTX (mode)`` refers to an expression with
value 0 in mode :samp:`{mode}`.  If mode :samp:`{mode}` is of mode class
``MODE_INT``, it returns ``const0_rtx``.  If mode :samp:`{mode}` is of
mode class ``MODE_FLOAT``, it returns a ``CONST_DOUBLE``
expression in mode :samp:`{mode}`.  Otherwise, it returns a
``CONST_VECTOR`` expression in mode :samp:`{mode}`.  Similarly, the macro
``CONST1_RTX (mode)`` refers to an expression with value 1 in
mode :samp:`{mode}` and similarly for ``CONST2_RTX``.  The
``CONST1_RTX`` and ``CONST2_RTX`` macros are undefined
for vector modes.