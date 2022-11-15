..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RTL classes, classes of RTX codes, RTX codes, classes of, GET_RTX_CLASS

.. _rtl-classes:

RTL Classes and Formats
***********************

The various expression codes are divided into several :dfn:`classes`,
which are represented by single characters.  You can determine the class
of an RTX code with the macro ``GET_RTX_CLASS (code)``.
Currently, :samp:`rtl.def` defines these classes:

.. envvar:: RTX_OBJ

  An RTX code that represents an actual object, such as a register
  (``REG``) or a memory location (``MEM``, ``SYMBOL_REF``).
  ``LO_SUM`` is also included; instead, ``SUBREG`` and
  ``STRICT_LOW_PART`` are not in this class, but in class
  ``RTX_EXTRA``.

.. envvar:: RTX_CONST_OBJ

  An RTX code that represents a constant object.  ``HIGH`` is also
  included in this class.

.. envvar:: RTX_COMPARE

  An RTX code for a non-symmetric comparison, such as ``GEU`` or
  ``LT``.

.. envvar:: RTX_COMM_COMPARE

  An RTX code for a symmetric (commutative) comparison, such as ``EQ``
  or ``ORDERED``.

.. envvar:: RTX_UNARY

  An RTX code for a unary arithmetic operation, such as ``NEG``,
  ``NOT``, or ``ABS``.  This category also includes value extension
  (sign or zero) and conversions between integer and floating point.

.. envvar:: RTX_COMM_ARITH

  An RTX code for a commutative binary operation, such as ``PLUS`` or
  ``AND``.  ``NE`` and ``EQ`` are comparisons, so they have class
  ``RTX_COMM_COMPARE``.

.. envvar:: RTX_BIN_ARITH

  An RTX code for a non-commutative binary operation, such as ``MINUS``,
  ``DIV``, or ``ASHIFTRT``.

.. envvar:: RTX_BITFIELD_OPS

  An RTX code for a bit-field operation.  Currently only
  ``ZERO_EXTRACT`` and ``SIGN_EXTRACT``.  These have three inputs
  and are lvalues (so they can be used for insertion as well).
  See :ref:`bit-fields`.

.. envvar:: RTX_TERNARY

  An RTX code for other three input operations.  Currently only
  ``IF_THEN_ELSE``,  ``VEC_MERGE``, ``SIGN_EXTRACT``,
  ``ZERO_EXTRACT``, and ``FMA``.

.. envvar:: RTX_INSN

  An RTX code for an entire instruction:  ``INSN``, ``JUMP_INSN``, and
  ``CALL_INSN``.  See :ref:`insns`.

.. envvar:: RTX_MATCH

  An RTX code for something that matches in insns, such as
  ``MATCH_DUP``.  These only occur in machine descriptions.

.. envvar:: RTX_AUTOINC

  An RTX code for an auto-increment addressing mode, such as
  ``POST_INC``.  :samp:`XEXP ({x}, 0)` gives the auto-modified
  register.

.. envvar:: RTX_EXTRA

  All other RTX codes.  This category includes the remaining codes used
  only in machine descriptions (``DEFINE_*``, etc.).  It also includes
  all the codes describing side effects (``SET``, ``USE``,
  ``CLOBBER``, etc.) and the non-insns that may appear on an insn
  chain, such as ``NOTE``, ``BARRIER``, and ``CODE_LABEL``.
  ``SUBREG`` is also part of this class.

.. index:: RTL format

For each expression code, :samp:`rtl.def` specifies the number of
contained objects and their kinds using a sequence of characters
called the :dfn:`format` of the expression code.  For example,
the format of ``subreg`` is :samp:`ep`.

.. index:: RTL format characters

These are the most commonly used format characters:

``e``
  An expression (actually a pointer to an expression).

``i``
  An integer.

``w``
  A wide integer.

``s``
  A string.

``E``
  A vector of expressions.

  A few other format characters are used occasionally:

``u``
  :samp:`u` is equivalent to :samp:`e` except that it is printed differently
  in debugging dumps.  It is used for pointers to insns.

``n``
  :samp:`n` is equivalent to :samp:`i` except that it is printed differently
  in debugging dumps.  It is used for the line number or code number of a
  ``note`` insn.

``S``
  :samp:`S` indicates a string which is optional.  In the RTL objects in
  core, :samp:`S` is equivalent to :samp:`s`, but when the object is read,
  from an :samp:`md` file, the string value of this operand may be omitted.
  An omitted string is taken to be the null string.

``V``
  :samp:`V` indicates a vector which is optional.  In the RTL objects in
  core, :samp:`V` is equivalent to :samp:`E`, but when the object is read
  from an :samp:`md` file, the vector value of this operand may be omitted.
  An omitted vector is effectively the same as a vector of no elements.

``B``
  :samp:`B` indicates a pointer to basic block structure.

``p``
  A polynomial integer.  At present this is used only for ``SUBREG_BYTE``.

``0``
  :samp:`0` means a slot whose contents do not fit any normal category.
  :samp:`0` slots are not printed at all in dumps, and are often used in
  special ways by small parts of the compiler.

There are macros to get the number of operands and the format
of an expression code:

.. index:: GET_RTX_LENGTH

:samp:`GET_RTX_LENGTH ({code})`
  Number of operands of an RTX of code :samp:`{code}`.

  .. index:: GET_RTX_FORMAT

:samp:`GET_RTX_FORMAT ({code})`
  The format of an RTX of code :samp:`{code}`, as a C string.

Some classes of RTX codes always have the same format.  For example, it
is safe to assume that all comparison operations have format ``ee``.

.. envvar:: RTX_UNARY

  All codes of this class have format ``e``.

.. envvar:: RTX_BIN_ARITH

  All codes of these classes have format ``ee``.

.. envvar:: RTX_BITFIELD_OPS

  All codes of these classes have format ``eee``.

.. envvar:: RTX_INSN

  All codes of this class have formats that begin with ``iuueiee``.
  See :ref:`insns`.  Note that not all RTL objects linked onto an insn chain
  are of class ``RTX_INSN``.

.. envvar:: RTX_CONST_OBJ

  You can make no assumptions about the format of these codes.
