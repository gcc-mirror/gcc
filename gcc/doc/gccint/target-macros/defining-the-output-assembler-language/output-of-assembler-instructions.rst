..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _instruction-output:

Output of Assembler Instructions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This describes assembler instruction output.

.. c:macro:: REGISTER_NAMES

  A C initializer containing the assembler's names for the machine
  registers, each one as a C string constant.  This is what translates
  register numbers in the compiler into assembler language.

.. c:macro:: ADDITIONAL_REGISTER_NAMES

  If defined, a C initializer for an array of structures containing a name
  and a register number.  This macro defines additional names for hard
  registers, thus allowing the ``asm`` option in declarations to refer
  to registers using alternate names.

.. c:macro:: OVERLAPPING_REGISTER_NAMES

  If defined, a C initializer for an array of structures containing a
  name, a register number and a count of the number of consecutive
  machine registers the name overlaps.  This macro defines additional
  names for hard registers, thus allowing the ``asm`` option in
  declarations to refer to registers using alternate names.  Unlike
  ``ADDITIONAL_REGISTER_NAMES``, this macro should be used when the
  register name implies multiple underlying registers.

  This macro should be used when it is important that a clobber in an
  ``asm`` statement clobbers all the underlying values implied by the
  register name.  For example, on ARM, clobbering the double-precision
  VFP register 'd0' implies clobbering both single-precision registers
  's0' and 's1'.

.. c:macro:: ASM_OUTPUT_OPCODE (stream, ptr)

  Define this macro if you are using an unusual assembler that
  requires different names for the machine instructions.

  The definition is a C statement or statements which output an
  assembler instruction opcode to the stdio stream :samp:`{stream}`.  The
  macro-operand :samp:`{ptr}` is a variable of type ``char *`` which
  points to the opcode name in its 'internal' form---the form that is
  written in the machine description.  The definition should output the
  opcode name to :samp:`{stream}`, performing any translation you desire, and
  increment the variable :samp:`{ptr}` to point at the end of the opcode
  so that it will not be output twice.

  In fact, your macro definition may process less than the entire opcode
  name, or more than the opcode name; but if you want to process text
  that includes :samp:`%`-sequences to substitute operands, you must take
  care of the substitution yourself.  Just be sure to increment
  :samp:`{ptr}` over whatever text should not be output normally.

  .. index:: recog_data.operand

  If you need to look at the operand values, they can be found as the
  elements of ``recog_data.operand``.

  If the macro definition does nothing, the instruction is output
  in the usual way.

.. c:macro:: FINAL_PRESCAN_INSN (insn, opvec, noperands)

  If defined, a C statement to be executed just prior to the output of
  assembler code for :samp:`{insn}`, to modify the extracted operands so
  they will be output differently.

  Here the argument :samp:`{opvec}` is the vector containing the operands
  extracted from :samp:`{insn}`, and :samp:`{noperands}` is the number of
  elements of the vector which contain meaningful data for this insn.
  The contents of this vector are what will be used to convert the insn
  template into assembler code, so you can change the assembler output
  by changing the contents of the vector.

  This macro is useful when various assembler syntaxes share a single
  file of instruction patterns; by defining this macro differently, you
  can cause a large class of instructions to be output differently (such
  as with rearranged operands).  Naturally, variations in assembler
  syntax affecting individual insn patterns ought to be handled by
  writing conditional output routines in those patterns.

  If this macro is not defined, it is equivalent to a null statement.

.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_FINAL_POSTSCAN_INSN]
  :end-before: [TARGET_ASM_FINAL_POSTSCAN_INSN]


.. c:macro:: PRINT_OPERAND (stream, x, code)

  A C compound statement to output to stdio stream :samp:`{stream}` the
  assembler syntax for an instruction operand :samp:`{x}`.  :samp:`{x}` is an
  RTL expression.

  :samp:`{code}` is a value that can be used to specify one of several ways
  of printing the operand.  It is used when identical operands must be
  printed differently depending on the context.  :samp:`{code}` comes from
  the :samp:`%` specification that was used to request printing of the
  operand.  If the specification was just :samp:`%{digit}` then
  :samp:`{code}` is 0; if the specification was :samp:`%{ltr}{digit}` then :samp:`{code}` is the ASCII code for :samp:`{ltr}`.

  .. index:: reg_names

  If :samp:`{x}` is a register, this macro should print the register's name.
  The names can be found in an array ``reg_names`` whose type is
  ``char *[]``.  ``reg_names`` is initialized from
  ``REGISTER_NAMES``.

  When the machine description has a specification :samp:`%{punct}`
  (a :samp:`%` followed by a punctuation character), this macro is called
  with a null pointer for :samp:`{x}` and the punctuation character for
  :samp:`{code}`.

.. c:macro:: PRINT_OPERAND_PUNCT_VALID_P (code)

  A C expression which evaluates to true if :samp:`{code}` is a valid
  punctuation character for use in the ``PRINT_OPERAND`` macro.  If
  ``PRINT_OPERAND_PUNCT_VALID_P`` is not defined, it means that no
  punctuation characters (except for the standard one, :samp:`%`) are used
  in this way.

.. c:macro:: PRINT_OPERAND_ADDRESS (stream, x)

  A C compound statement to output to stdio stream :samp:`{stream}` the
  assembler syntax for an instruction operand that is a memory reference
  whose address is :samp:`{x}`.  :samp:`{x}` is an RTL expression.

  .. index:: TARGET_ENCODE_SECTION_INFO usage

  On some machines, the syntax for a symbolic address depends on the
  section that the address refers to.  On these machines, define the hook
  ``TARGET_ENCODE_SECTION_INFO`` to store the information into the
  ``symbol_ref``, and then check for it here.  See :ref:`assembler-format`.

.. index:: dbr_sequence_length

.. c:macro:: DBR_OUTPUT_SEQEND (file)

  A C statement, to be executed after all slot-filler instructions have
  been output.  If necessary, call ``dbr_sequence_length`` to
  determine the number of slots filled in a sequence (zero if not
  currently outputting a sequence), to decide how many no-ops to output,
  or whatever.

  Don't define this macro if it has nothing to do, but it is helpful in
  reading assembly output if the extent of the delay sequence is made
  explicit (e.g. with white space).

.. index:: final_sequence

Note that output routines for instructions with delay slots must be
prepared to deal with not being output as part of a sequence
(i.e. when the scheduling pass is not run, or when no slot fillers could be
found.)  The variable ``final_sequence`` is null when not
processing a sequence, otherwise it contains the ``sequence`` rtx
being output.

.. index:: asm_fprintf

.. c:macro:: REGISTER_PREFIX
             LOCAL_LABEL_PREFIX
             USER_LABEL_PREFIX
             IMMEDIATE_PREFIX

  If defined, C string expressions to be used for the :samp:`%R`, :samp:`%L`,
  :samp:`%U`, and :samp:`%I` options of ``asm_fprintf`` (see
  :samp:`final.cc`).  These are useful when a single :samp:`md` file must
  support multiple assembler formats.  In that case, the various :samp:`tm.h`
  files can define these macros differently.

.. c:macro:: ASM_FPRINTF_EXTENSIONS (file, argptr, format)

  If defined this macro should expand to a series of ``case``
  statements which will be parsed inside the ``switch`` statement of
  the ``asm_fprintf`` function.  This allows targets to define extra
  printf formats which may useful when generating their assembler
  statements.  Note that uppercase letters are reserved for future
  generic extensions to asm_fprintf, and so are not available to target
  specific code.  The output file is given by the parameter :samp:`{file}`.
  The varargs input pointer is :samp:`{argptr}` and the rest of the format
  string, starting the character after the one that is being switched
  upon, is pointed to by :samp:`{format}`.

.. c:macro:: ASSEMBLER_DIALECT

  If your target supports multiple dialects of assembler language (such as
  different opcodes), define this macro as a C expression that gives the
  numeric index of the assembler language dialect to use, with zero as the
  first variant.

  If this macro is defined, you may use constructs of the form

  .. code-block:: c++

    {option0|option1|option2...}

  in the output templates of patterns (see :ref:`output-template`) or in the
  first argument of ``asm_fprintf``.  This construct outputs
  :samp:`option0`, :samp:`option1`, :samp:`option2`, etc., if the value of
  ``ASSEMBLER_DIALECT`` is zero, one, two, etc.  Any special characters
  within these strings retain their usual meaning.  If there are fewer
  alternatives within the braces than the value of
  ``ASSEMBLER_DIALECT``, the construct outputs nothing. If it's needed
  to print curly braces or :samp:`|` character in assembler output directly,
  :samp:`%{`, :samp:`%}` and :samp:`%|` can be used.

  If you do not define this macro, the characters :samp:`{`, :samp:`|` and
  :samp:`}` do not have any special meaning when used in templates or
  operands to ``asm_fprintf``.

  Define the macros ``REGISTER_PREFIX``, ``LOCAL_LABEL_PREFIX``,
  ``USER_LABEL_PREFIX`` and ``IMMEDIATE_PREFIX`` if you can express
  the variations in assembler language syntax with that mechanism.  Define
  ``ASSEMBLER_DIALECT`` and use the :samp:`{option0|option1}` syntax
  if the syntax variant are larger and involve such things as different
  opcodes or operand order.

.. c:macro:: ASM_OUTPUT_REG_PUSH (stream, regno)

  A C expression to output to :samp:`{stream}` some assembler code
  which will push hard register number :samp:`{regno}` onto the stack.
  The code need not be optimal, since this macro is used only when
  profiling.

.. c:macro:: ASM_OUTPUT_REG_POP (stream, regno)

  A C expression to output to :samp:`{stream}` some assembler code
  which will pop hard register number :samp:`{regno}` off of the stack.
  The code need not be optimal, since this macro is used only when
  profiling.
