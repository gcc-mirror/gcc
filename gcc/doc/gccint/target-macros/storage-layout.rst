..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: storage layout

.. _storage-layout:

Storage Layout
**************

Note that the definitions of the macros in this table which are sizes or
alignments measured in bits do not need to be constant.  They can be C
expressions that refer to static variables, such as the ``target_flags``.
See :ref:`run-time-target`.

.. c:macro:: BITS_BIG_ENDIAN

  Define this macro to have the value 1 if the most significant bit in a
  byte has the lowest number; otherwise define it to have the value zero.
  This means that bit-field instructions count from the most significant
  bit.  If the machine has no bit-field instructions, then this must still
  be defined, but it doesn't matter which value it is defined to.  This
  macro need not be a constant.

  This macro does not affect the way structure fields are packed into
  bytes or words; that is controlled by ``BYTES_BIG_ENDIAN``.

.. c:macro:: BYTES_BIG_ENDIAN

  Define this macro to have the value 1 if the most significant byte in a
  word has the lowest number.  This macro need not be a constant.

.. c:macro:: WORDS_BIG_ENDIAN

  Define this macro to have the value 1 if, in a multiword object, the
  most significant word has the lowest number.  This applies to both
  memory locations and registers; see ``REG_WORDS_BIG_ENDIAN`` if the
  order of words in memory is not the same as the order in registers.  This
  macro need not be a constant.

.. c:macro:: REG_WORDS_BIG_ENDIAN

  On some machines, the order of words in a multiword object differs between
  registers in memory.  In such a situation, define this macro to describe
  the order of words in a register.  The macro ``WORDS_BIG_ENDIAN`` controls
  the order of words in memory.

.. c:macro:: FLOAT_WORDS_BIG_ENDIAN

  Define this macro to have the value 1 if ``DFmode``, ``XFmode`` or
  ``TFmode`` floating point numbers are stored in memory with the word
  containing the sign bit at the lowest address; otherwise define it to
  have the value 0.  This macro need not be a constant.

  You need not define this macro if the ordering is the same as for
  multi-word integers.

.. c:macro:: BITS_PER_WORD

  Number of bits in a word.  If you do not define this macro, the default
  is ``BITS_PER_UNIT * UNITS_PER_WORD``.

.. c:macro:: MAX_BITS_PER_WORD

  Maximum number of bits in a word.  If this is undefined, the default is
  ``BITS_PER_WORD``.  Otherwise, it is the constant value that is the
  largest value that ``BITS_PER_WORD`` can have at run-time.

.. c:macro:: UNITS_PER_WORD

  Number of storage units in a word; normally the size of a general-purpose
  register, a power of two from 1 or 8.

.. c:macro:: MIN_UNITS_PER_WORD

  Minimum number of units in a word.  If this is undefined, the default is
  ``UNITS_PER_WORD``.  Otherwise, it is the constant value that is the
  smallest value that ``UNITS_PER_WORD`` can have at run-time.

.. c:macro:: POINTER_SIZE

  Width of a pointer, in bits.  You must specify a value no wider than the
  width of ``Pmode``.  If it is not equal to the width of ``Pmode``,
  you must define ``POINTERS_EXTEND_UNSIGNED``.  If you do not specify
  a value the default is ``BITS_PER_WORD``.

.. c:macro:: POINTERS_EXTEND_UNSIGNED

  A C expression that determines how pointers should be extended from
  ``ptr_mode`` to either ``Pmode`` or ``word_mode``.  It is
  greater than zero if pointers should be zero-extended, zero if they
  should be sign-extended, and negative if some other sort of conversion
  is needed.  In the last case, the extension is done by the target's
  ``ptr_extend`` instruction.

  You need not define this macro if the ``ptr_mode``, ``Pmode``
  and ``word_mode`` are all the same width.

.. c:macro:: PROMOTE_MODE (m, unsignedp, type)

  A macro to update :samp:`{m}` and :samp:`{unsignedp}` when an object whose type
  is :samp:`{type}` and which has the specified mode and signedness is to be
  stored in a register.  This macro is only called when :samp:`{type}` is a
  scalar type.

  On most RISC machines, which only have operations that operate on a full
  register, define this macro to set :samp:`{m}` to ``word_mode`` if
  :samp:`{m}` is an integer mode narrower than ``BITS_PER_WORD``.  In most
  cases, only integer modes should be widened because wider-precision
  floating-point operations are usually more expensive than their narrower
  counterparts.

  For most machines, the macro definition does not change :samp:`{unsignedp}`.
  However, some machines, have instructions that preferentially handle
  either signed or unsigned quantities of certain modes.  For example, on
  the DEC Alpha, 32-bit loads from memory and 32-bit add instructions
  sign-extend the result to 64 bits.  On such machines, set
  :samp:`{unsignedp}` according to which kind of extension is more efficient.

  Do not define this macro if it would never modify :samp:`{m}`.

.. include:: tm.rst.in
  :start-after: [TARGET_C_EXCESS_PRECISION]
  :end-before: [TARGET_C_EXCESS_PRECISION]


.. include:: tm.rst.in
  :start-after: [TARGET_PROMOTE_FUNCTION_MODE]
  :end-before: [TARGET_PROMOTE_FUNCTION_MODE]


.. c:macro:: PARM_BOUNDARY

  Normal alignment required for function parameters on the stack, in
  bits.  All stack parameters receive at least this much alignment
  regardless of data type.  On most machines, this is the same as the
  size of an integer.

.. c:macro:: STACK_BOUNDARY

  Define this macro to the minimum alignment enforced by hardware for the
  stack pointer on this machine.  The definition is a C expression for the
  desired alignment (measured in bits).  This value is used as a default
  if ``PREFERRED_STACK_BOUNDARY`` is not defined.  On most machines,
  this should be the same as ``PARM_BOUNDARY``.

.. c:macro:: PREFERRED_STACK_BOUNDARY

  Define this macro if you wish to preserve a certain alignment for the
  stack pointer, greater than what the hardware enforces.  The definition
  is a C expression for the desired alignment (measured in bits).  This
  macro must evaluate to a value equal to or larger than
  ``STACK_BOUNDARY``.

.. c:macro:: INCOMING_STACK_BOUNDARY

  Define this macro if the incoming stack boundary may be different
  from ``PREFERRED_STACK_BOUNDARY``.  This macro must evaluate
  to a value equal to or larger than ``STACK_BOUNDARY``.

.. c:macro:: FUNCTION_BOUNDARY

  Alignment required for a function entry point, in bits.

.. c:macro:: BIGGEST_ALIGNMENT

  Biggest alignment that any data type can require on this machine, in
  bits.  Note that this is not the biggest alignment that is supported,
  just the biggest alignment that, when violated, may cause a fault.

.. include:: tm.rst.in
  :start-after: [TARGET_ABSOLUTE_BIGGEST_ALIGNMENT]
  :end-before: [TARGET_ABSOLUTE_BIGGEST_ALIGNMENT]


.. c:macro:: MALLOC_ABI_ALIGNMENT

  Alignment, in bits, a C conformant malloc implementation has to
  provide.  If not defined, the default value is ``BITS_PER_WORD``.

.. c:macro:: ATTRIBUTE_ALIGNED_VALUE

  Alignment used by the ``__attribute__ ((aligned))`` construct.  If
  not defined, the default value is ``BIGGEST_ALIGNMENT``.

.. c:macro:: MINIMUM_ATOMIC_ALIGNMENT

  If defined, the smallest alignment, in bits, that can be given to an
  object that can be referenced in one operation, without disturbing any
  nearby object.  Normally, this is ``BITS_PER_UNIT``, but may be larger
  on machines that don't have byte or half-word store operations.

.. c:macro:: BIGGEST_FIELD_ALIGNMENT

  Biggest alignment that any structure or union field can require on this
  machine, in bits.  If defined, this overrides ``BIGGEST_ALIGNMENT`` for
  structure and union fields only, unless the field alignment has been set
  by the ``__attribute__ ((aligned (n)))`` construct.

.. c:macro:: ADJUST_FIELD_ALIGN (field, type, computed)

  An expression for the alignment of a structure field :samp:`{field}` of
  type :samp:`{type}` if the alignment computed in the usual way (including
  applying of ``BIGGEST_ALIGNMENT`` and ``BIGGEST_FIELD_ALIGNMENT`` to the
  alignment) is :samp:`{computed}`.  It overrides alignment only if the
  field alignment has not been set by the
  ``__attribute__ ((aligned (n)))`` construct.  Note that :samp:`{field}`
  may be ``NULL_TREE`` in case we just query for the minimum alignment
  of a field of type :samp:`{type}` in structure context.

.. c:macro:: MAX_STACK_ALIGNMENT

  Biggest stack alignment guaranteed by the backend.  Use this macro
  to specify the maximum alignment of a variable on stack.

  If not defined, the default value is ``STACK_BOUNDARY``.

  .. todo:: The default should be @code{PREFERRED_STACK_BOUNDARY}.
    But the fix for PR 32893 indicates that we can only guarantee
    maximum stack alignment on stack up to @code{STACK_BOUNDARY}, not
    @code{PREFERRED_STACK_BOUNDARY}, if stack alignment isn't supported.

.. c:macro:: MAX_OFILE_ALIGNMENT

  Biggest alignment supported by the object file format of this machine.
  Use this macro to limit the alignment which can be specified using the
  ``__attribute__ ((aligned (n)))`` construct for functions and
  objects with static storage duration.  The alignment of automatic
  objects may exceed the object file format maximum up to the maximum
  supported by GCC.  If not defined, the default value is
  ``BIGGEST_ALIGNMENT``.

  On systems that use ELF, the default (in :samp:`config/elfos.h`) is
  the largest supported 32-bit ELF section alignment representable on
  a 32-bit host e.g. :samp:`(((uint64_t) 1 << 28) * 8)`.
  On 32-bit ELF the largest supported section alignment in bits is
  :samp:`(0x80000000 * 8)`, but this is not representable on 32-bit hosts.

.. include:: tm.rst.in
  :start-after: [TARGET_LOWER_LOCAL_DECL_ALIGNMENT]
  :end-before: [TARGET_LOWER_LOCAL_DECL_ALIGNMENT]


.. include:: tm.rst.in
  :start-after: [TARGET_STATIC_RTX_ALIGNMENT]
  :end-before: [TARGET_STATIC_RTX_ALIGNMENT]


.. c:macro:: DATA_ALIGNMENT (type, basic_align)

  If defined, a C expression to compute the alignment for a variable in
  the static store.  :samp:`{type}` is the data type, and :samp:`{basic_align}` is
  the alignment that the object would ordinarily have.  The value of this
  macro is used instead of that alignment to align the object.

  If this macro is not defined, then :samp:`{basic_align}` is used.

  .. index:: strcpy

  One use of this macro is to increase alignment of medium-size data to
  make it all fit in fewer cache lines.  Another is to cause character
  arrays to be word-aligned so that ``strcpy`` calls that copy
  constants to character arrays can be done inline.

.. c:macro:: DATA_ABI_ALIGNMENT (type, basic_align)

  Similar to ``DATA_ALIGNMENT``, but for the cases where the ABI mandates
  some alignment increase, instead of optimization only purposes.  E.g.AMD x86-64 psABI says that variables with array type larger than 15 bytes
  must be aligned to 16 byte boundaries.

  If this macro is not defined, then :samp:`{basic_align}` is used.

.. include:: tm.rst.in
  :start-after: [TARGET_CONSTANT_ALIGNMENT]
  :end-before: [TARGET_CONSTANT_ALIGNMENT]


.. c:macro:: LOCAL_ALIGNMENT (type, basic_align)

  If defined, a C expression to compute the alignment for a variable in
  the local store.  :samp:`{type}` is the data type, and :samp:`{basic_align}` is
  the alignment that the object would ordinarily have.  The value of this
  macro is used instead of that alignment to align the object.

  If this macro is not defined, then :samp:`{basic_align}` is used.

  One use of this macro is to increase alignment of medium-size data to
  make it all fit in fewer cache lines.

  If the value of this macro has a type, it should be an unsigned type.

.. include:: tm.rst.in
  :start-after: [TARGET_VECTOR_ALIGNMENT]
  :end-before: [TARGET_VECTOR_ALIGNMENT]


.. c:macro:: STACK_SLOT_ALIGNMENT (type, mode, basic_align)

  If defined, a C expression to compute the alignment for stack slot.
  :samp:`{type}` is the data type, :samp:`{mode}` is the widest mode available,
  and :samp:`{basic_align}` is the alignment that the slot would ordinarily
  have.  The value of this macro is used instead of that alignment to
  align the slot.

  If this macro is not defined, then :samp:`{basic_align}` is used when
  :samp:`{type}` is ``NULL``.  Otherwise, ``LOCAL_ALIGNMENT`` will
  be used.

  This macro is to set alignment of stack slot to the maximum alignment
  of all possible modes which the slot may have.

  If the value of this macro has a type, it should be an unsigned type.

.. c:macro:: LOCAL_DECL_ALIGNMENT (decl)

  If defined, a C expression to compute the alignment for a local
  variable :samp:`{decl}`.

  If this macro is not defined, then
  ``LOCAL_ALIGNMENT (TREE_TYPE (decl), DECL_ALIGN (decl))``
  is used.

  One use of this macro is to increase alignment of medium-size data to
  make it all fit in fewer cache lines.

  If the value of this macro has a type, it should be an unsigned type.

.. c:macro:: MINIMUM_ALIGNMENT (exp, mode, align)

  If defined, a C expression to compute the minimum required alignment
  for dynamic stack realignment purposes for :samp:`{exp}` (a type or decl),
  :samp:`{mode}`, assuming normal alignment :samp:`{align}`.

  If this macro is not defined, then :samp:`{align}` will be used.

.. c:macro:: EMPTY_FIELD_BOUNDARY

  Alignment in bits to be given to a structure bit-field that follows an
  empty field such as ``int : 0;``.

  If ``PCC_BITFIELD_TYPE_MATTERS`` is true, it overrides this macro.

.. c:macro:: STRUCTURE_SIZE_BOUNDARY

  Number of bits which any structure or union's size must be a multiple of.
  Each structure or union's size is rounded up to a multiple of this.

  If you do not define this macro, the default is the same as
  ``BITS_PER_UNIT``.

.. c:macro:: STRICT_ALIGNMENT

  Define this macro to be the value 1 if instructions will fail to work
  if given data not on the nominal alignment.  If instructions will merely
  go slower in that case, define this macro as 0.

.. c:macro:: PCC_BITFIELD_TYPE_MATTERS

  Define this if you wish to imitate the way many other C compilers handle
  alignment of bit-fields and the structures that contain them.

  The behavior is that the type written for a named bit-field (``int``,
  ``short``, or other integer type) imposes an alignment for the entire
  structure, as if the structure really did contain an ordinary field of
  that type.  In addition, the bit-field is placed within the structure so
  that it would fit within such a field, not crossing a boundary for it.

  Thus, on most machines, a named bit-field whose type is written as
  ``int`` would not cross a four-byte boundary, and would force
  four-byte alignment for the whole structure.  (The alignment used may
  not be four bytes; it is controlled by the other alignment parameters.)

  An unnamed bit-field will not affect the alignment of the containing
  structure.

  If the macro is defined, its definition should be a C expression;
  a nonzero value for the expression enables this behavior.

  Note that if this macro is not defined, or its value is zero, some
  bit-fields may cross more than one alignment boundary.  The compiler can
  support such references if there are :samp:`insv`, :samp:`extv`, and
  :samp:`extzv` insns that can directly reference memory.

  The other known way of making bit-fields work is to define
  ``STRUCTURE_SIZE_BOUNDARY`` as large as ``BIGGEST_ALIGNMENT``.
  Then every structure can be accessed with fullwords.

  Unless the machine has bit-field instructions or you define
  ``STRUCTURE_SIZE_BOUNDARY`` that way, you must define
  ``PCC_BITFIELD_TYPE_MATTERS`` to have a nonzero value.

  If your aim is to make GCC use the same conventions for laying out
  bit-fields as are used by another compiler, here is how to investigate
  what the other compiler does.  Compile and run this program:

  .. code-block:: c++

    struct foo1
    {
      char x;
      char :0;
      char y;
    };

    struct foo2
    {
      char x;
      int :0;
      char y;
    };

    main ()
    {
      printf ("Size of foo1 is %d\n",
              sizeof (struct foo1));
      printf ("Size of foo2 is %d\n",
              sizeof (struct foo2));
      exit (0);
    }

  If this prints 2 and 5, then the compiler's behavior is what you would
  get from ``PCC_BITFIELD_TYPE_MATTERS``.

.. c:macro:: BITFIELD_NBYTES_LIMITED

  Like ``PCC_BITFIELD_TYPE_MATTERS`` except that its effect is limited
  to aligning a bit-field within the structure.

.. include:: tm.rst.in
  :start-after: [TARGET_ALIGN_ANON_BITFIELD]
  :end-before: [TARGET_ALIGN_ANON_BITFIELD]


.. include:: tm.rst.in
  :start-after: [TARGET_NARROW_VOLATILE_BITFIELD]
  :end-before: [TARGET_NARROW_VOLATILE_BITFIELD]


.. include:: tm.rst.in
  :start-after: [TARGET_MEMBER_TYPE_FORCES_BLK]
  :end-before: [TARGET_MEMBER_TYPE_FORCES_BLK]


.. c:macro:: ROUND_TYPE_ALIGN (type, computed, specified)

  Define this macro as an expression for the alignment of a type (given
  by :samp:`{type}` as a tree node) if the alignment computed in the usual
  way is :samp:`{computed}` and the alignment explicitly specified was
  :samp:`{specified}`.

  The default is to use :samp:`{specified}` if it is larger; otherwise, use
  the smaller of :samp:`{computed}` and ``BIGGEST_ALIGNMENT``

.. c:macro:: MAX_FIXED_MODE_SIZE

  An integer expression for the size in bits of the largest integer
  machine mode that should actually be used.  All integer machine modes of
  this size or smaller can be used for structures and unions with the
  appropriate sizes.  If this macro is undefined, ``GET_MODE_BITSIZE
  (DImode)`` is assumed.

.. c:macro:: STACK_SAVEAREA_MODE (save_level)

  If defined, an expression of type ``machine_mode`` that
  specifies the mode of the save area operand of a
  ``save_stack_level`` named pattern (see :ref:`standard-names`).
  :samp:`{save_level}` is one of ``SAVE_BLOCK``, ``SAVE_FUNCTION``, or
  ``SAVE_NONLOCAL`` and selects which of the three named patterns is
  having its mode specified.

  You need not define this macro if it always returns ``Pmode``.  You
  would most commonly define this macro if the
  ``save_stack_level`` patterns need to support both a 32- and a
  64-bit mode.

.. c:macro:: STACK_SIZE_MODE

  If defined, an expression of type ``machine_mode`` that
  specifies the mode of the size increment operand of an
  ``allocate_stack`` named pattern (see :ref:`standard-names`).

  You need not define this macro if it always returns ``word_mode``.
  You would most commonly define this macro if the ``allocate_stack``
  pattern needs to support both a 32- and a 64-bit mode.

.. include:: tm.rst.in
  :start-after: [TARGET_LIBGCC_CMP_RETURN_MODE]
  :end-before: [TARGET_LIBGCC_CMP_RETURN_MODE]


.. include:: tm.rst.in
  :start-after: [TARGET_LIBGCC_SHIFT_COUNT_MODE]
  :end-before: [TARGET_LIBGCC_SHIFT_COUNT_MODE]


.. include:: tm.rst.in
  :start-after: [TARGET_UNWIND_WORD_MODE]
  :end-before: [TARGET_UNWIND_WORD_MODE]


.. include:: tm.rst.in
  :start-after: [TARGET_MS_BITFIELD_LAYOUT_P]
  :end-before: [TARGET_MS_BITFIELD_LAYOUT_P]


.. include:: tm.rst.in
  :start-after: [TARGET_DECIMAL_FLOAT_SUPPORTED_P]
  :end-before: [TARGET_DECIMAL_FLOAT_SUPPORTED_P]


.. include:: tm.rst.in
  :start-after: [TARGET_FIXED_POINT_SUPPORTED_P]
  :end-before: [TARGET_FIXED_POINT_SUPPORTED_P]


.. include:: tm.rst.in
  :start-after: [TARGET_EXPAND_TO_RTL_HOOK]
  :end-before: [TARGET_EXPAND_TO_RTL_HOOK]


.. include:: tm.rst.in
  :start-after: [TARGET_INSTANTIATE_DECLS]
  :end-before: [TARGET_INSTANTIATE_DECLS]


.. include:: tm.rst.in
  :start-after: [TARGET_MANGLE_TYPE]
  :end-before: [TARGET_MANGLE_TYPE]
