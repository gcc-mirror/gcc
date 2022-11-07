..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _data-output:

Output of Data
^^^^^^^^^^^^^^

.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_BYTE_OP]
  :end-before: [TARGET_ASM_BYTE_OP]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_INTEGER]
  :end-before: [TARGET_ASM_INTEGER]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_DECL_END]
  :end-before: [TARGET_ASM_DECL_END]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA]
  :end-before: [TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA]


.. c:macro:: ASM_OUTPUT_ASCII (stream, ptr, len)

  A C statement to output to the stdio stream :samp:`{stream}` an assembler
  instruction to assemble a string constant containing the :samp:`{len}`
  bytes at :samp:`{ptr}`.  :samp:`{ptr}` will be a C expression of type
  ``char *`` and :samp:`{len}` a C expression of type ``int``.

  If the assembler has a ``.ascii`` pseudo-op as found in the
  Berkeley Unix assembler, do not define the macro
  ``ASM_OUTPUT_ASCII``.

.. c:macro:: ASM_OUTPUT_FDESC (stream, decl, n)

  A C statement to output word :samp:`{n}` of a function descriptor for
  :samp:`{decl}`.  This must be defined if ``TARGET_VTABLE_USES_DESCRIPTORS``
  is defined, and is otherwise unused.

.. c:macro:: CONSTANT_POOL_BEFORE_FUNCTION

  You may define this macro as a C expression.  You should define the
  expression to have a nonzero value if GCC should output the constant
  pool for a function before the code for the function, or a zero value if
  GCC should output the constant pool after the function.  If you do
  not define this macro, the usual case, GCC will output the constant
  pool before the function.

.. c:macro:: ASM_OUTPUT_POOL_PROLOGUE (file, funname, fundecl, size)

  A C statement to output assembler commands to define the start of the
  constant pool for a function.  :samp:`{funname}` is a string giving
  the name of the function.  Should the return type of the function
  be required, it can be obtained via :samp:`{fundecl}`.  :samp:`{size}`
  is the size, in bytes, of the constant pool that will be written
  immediately after this call.

  If no constant-pool prefix is required, the usual case, this macro need
  not be defined.

.. c:macro:: ASM_OUTPUT_SPECIAL_POOL_ENTRY (file, x, mode, align, labelno, jumpto)

  A C statement (with or without semicolon) to output a constant in the
  constant pool, if it needs special treatment.  (This macro need not do
  anything for RTL expressions that can be output normally.)

  The argument :samp:`{file}` is the standard I/O stream to output the
  assembler code on.  :samp:`{x}` is the RTL expression for the constant to
  output, and :samp:`{mode}` is the machine mode (in case :samp:`{x}` is a
  :samp:`const_int`).  :samp:`{align}` is the required alignment for the value
  :samp:`{x}` ; you should output an assembler directive to force this much
  alignment.

  The argument :samp:`{labelno}` is a number to use in an internal label for
  the address of this pool entry.  The definition of this macro is
  responsible for outputting the label definition at the proper place.
  Here is how to do this:

  .. code-block:: c++

    (*targetm.asm_out.internal_label) (file, "LC", labelno);

  When you output a pool entry specially, you should end with a
  ``goto`` to the label :samp:`{jumpto}`.  This will prevent the same pool
  entry from being output a second time in the usual manner.

  You need not define this macro if it would do nothing.

.. c:macro:: ASM_OUTPUT_POOL_EPILOGUE (file funname, fundecl size)

  A C statement to output assembler commands to at the end of the constant
  pool for a function.  :samp:`{funname}` is a string giving the name of the
  function.  Should the return type of the function be required, you can
  obtain it via :samp:`{fundecl}`.  :samp:`{size}` is the size, in bytes, of the
  constant pool that GCC wrote immediately before this call.

  If no constant-pool epilogue is required, the usual case, you need not
  define this macro.

.. c:macro:: IS_ASM_LOGICAL_LINE_SEPARATOR (C, STR)

  Define this macro as a C expression which is nonzero if :samp:`{C}` is
  used as a logical line separator by the assembler.  :samp:`{STR}` points
  to the position in the string where :samp:`{C}` was found; this can be used if
  a line separator uses multiple characters.

  If you do not define this macro, the default is that only
  the character :samp:`;` is treated as a logical line separator.

.. c:var:: const char * TARGET_ASM_OPEN_PAREN

.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_OPEN_PAREN]
  :end-before: [TARGET_ASM_OPEN_PAREN]


These macros are provided by :samp:`real.h` for writing the definitions
of ``ASM_OUTPUT_DOUBLE`` and the like:

.. c:macro:: REAL_VALUE_TO_TARGET_SINGLE (x, l)
             REAL_VALUE_TO_TARGET_DOUBLE (x, l)
             REAL_VALUE_TO_TARGET_LONG_DOUBLE (x, l)
             REAL_VALUE_TO_TARGET_DECIMAL32 (x, l)
             REAL_VALUE_TO_TARGET_DECIMAL64 (x, l)
             REAL_VALUE_TO_TARGET_DECIMAL128 (x, l)

  These translate :samp:`{x}`, of type ``REAL_VALUE_TYPE``, to the
  target's floating point representation, and store its bit pattern in
  the variable :samp:`{l}`.  For ``REAL_VALUE_TO_TARGET_SINGLE`` and
  ``REAL_VALUE_TO_TARGET_DECIMAL32``, this variable should be a
  simple ``long int``.  For the others, it should be an array of
  ``long int``.  The number of elements in this array is determined
  by the size of the desired target floating point data type: 32 bits of
  it go in each ``long int`` array element.  Each array element holds
  32 bits of the result, even if ``long int`` is wider than 32 bits
  on the host machine.

  The array element values are designed so that you can print them out
  using ``fprintf`` in the order they should appear in the target
  machine's memory.