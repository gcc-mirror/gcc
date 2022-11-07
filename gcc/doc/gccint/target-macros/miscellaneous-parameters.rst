..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: parameters, miscellaneous

.. _misc:

Miscellaneous Parameters
************************

.. prevent bad page break with this line

Here are several miscellaneous parameters.

.. c:macro:: HAS_LONG_COND_BRANCH

  Define this boolean macro to indicate whether or not your architecture
  has conditional branches that can span all of memory.  It is used in
  conjunction with an optimization that partitions hot and cold basic
  blocks into separate sections of the executable.  If this macro is
  set to false, gcc will convert any conditional branches that attempt
  to cross between sections into unconditional branches or indirect jumps.

.. c:macro:: HAS_LONG_UNCOND_BRANCH

  Define this boolean macro to indicate whether or not your architecture
  has unconditional branches that can span all of memory.  It is used in
  conjunction with an optimization that partitions hot and cold basic
  blocks into separate sections of the executable.  If this macro is
  set to false, gcc will convert any unconditional branches that attempt
  to cross between sections into indirect jumps.

.. c:macro:: CASE_VECTOR_MODE

  An alias for a machine mode name.  This is the machine mode that
  elements of a jump-table should have.

.. c:macro:: CASE_VECTOR_SHORTEN_MODE (min_offset, max_offset, body)

  Optional: return the preferred mode for an ``addr_diff_vec``
  when the minimum and maximum offset are known.  If you define this,
  it enables extra code in branch shortening to deal with ``addr_diff_vec``.
  To make this work, you also have to define ``INSN_ALIGN`` and
  make the alignment for ``addr_diff_vec`` explicit.
  The :samp:`{body}` argument is provided so that the offset_unsigned and scale
  flags can be updated.

.. c:macro:: CASE_VECTOR_PC_RELATIVE

  Define this macro to be a C expression to indicate when jump-tables
  should contain relative addresses.  You need not define this macro if
  jump-tables never contain relative addresses, or jump-tables should
  contain relative addresses only when :option:`-fPIC` or :option:`-fPIC`
  is in effect.

.. include:: tm.rst.in
  :start-after: [TARGET_CASE_VALUES_THRESHOLD]
  :end-before: [TARGET_CASE_VALUES_THRESHOLD]


.. c:macro:: WORD_REGISTER_OPERATIONS

  Define this macro to 1 if operations between registers with integral mode
  smaller than a word are always performed on the entire register.  To be
  more explicit, if you start with a pair of ``word_mode`` registers with
  known values and you do a subword, for example ``QImode``, addition on
  the low part of the registers, then the compiler may consider that the
  result has a known value in ``word_mode`` too if the macro is defined
  to 1.  Most RISC machines have this property and most CISC machines do not.

.. include:: tm.rst.in
  :start-after: [TARGET_MIN_ARITHMETIC_PRECISION]
  :end-before: [TARGET_MIN_ARITHMETIC_PRECISION]


.. c:macro:: LOAD_EXTEND_OP (mem_mode)

  Define this macro to be a C expression indicating when insns that read
  memory in :samp:`{mem_mode}`, an integral mode narrower than a word, set the
  bits outside of :samp:`{mem_mode}` to be either the sign-extension or the
  zero-extension of the data read.  Return ``SIGN_EXTEND`` for values
  of :samp:`{mem_mode}` for which the
  insn sign-extends, ``ZERO_EXTEND`` for which it zero-extends, and
  ``UNKNOWN`` for other modes.

  This macro is not called with :samp:`{mem_mode}` non-integral or with a width
  greater than or equal to ``BITS_PER_WORD``, so you may return any
  value in this case.  Do not define this macro if it would always return
  ``UNKNOWN``.  On machines where this macro is defined, you will normally
  define it as the constant ``SIGN_EXTEND`` or ``ZERO_EXTEND``.

  You may return a non- ``UNKNOWN`` value even if for some hard registers
  the sign extension is not performed, if for the ``REGNO_REG_CLASS``
  of these hard registers ``TARGET_CAN_CHANGE_MODE_CLASS`` returns false
  when the :samp:`{from}` mode is :samp:`{mem_mode}` and the :samp:`{to}` mode is any
  integral mode larger than this but not larger than ``word_mode``.

  You must return ``UNKNOWN`` if for some hard registers that allow this
  mode, ``TARGET_CAN_CHANGE_MODE_CLASS`` says that they cannot change to
  ``word_mode``, but that they can change to another integral mode that
  is larger then :samp:`{mem_mode}` but still smaller than ``word_mode``.

.. c:macro:: SHORT_IMMEDIATES_SIGN_EXTEND

  Define this macro to 1 if loading short immediate values into registers sign
  extends.

.. include:: tm.rst.in
  :start-after: [TARGET_MIN_DIVISIONS_FOR_RECIP_MUL]
  :end-before: [TARGET_MIN_DIVISIONS_FOR_RECIP_MUL]


.. c:macro:: MOVE_MAX

  The maximum number of bytes that a single instruction can move quickly
  between memory and registers or between two memory locations.

.. c:macro:: MAX_MOVE_MAX

  The maximum number of bytes that a single instruction can move quickly
  between memory and registers or between two memory locations.  If this
  is undefined, the default is ``MOVE_MAX``.  Otherwise, it is the
  constant value that is the largest value that ``MOVE_MAX`` can have
  at run-time.

.. c:macro:: SHIFT_COUNT_TRUNCATED

  A C expression that is nonzero if on this machine the number of bits
  actually used for the count of a shift operation is equal to the number
  of bits needed to represent the size of the object being shifted.  When
  this macro is nonzero, the compiler will assume that it is safe to omit
  a sign-extend, zero-extend, and certain bitwise 'and' instructions that
  truncates the count of a shift operation.  On machines that have
  instructions that act on bit-fields at variable positions, which may
  include 'bit test' instructions, a nonzero ``SHIFT_COUNT_TRUNCATED``
  also enables deletion of truncations of the values that serve as
  arguments to bit-field instructions.

  If both types of instructions truncate the count (for shifts) and
  position (for bit-field operations), or if no variable-position bit-field
  instructions exist, you should define this macro.

  However, on some machines, such as the 80386 and the 680x0, truncation
  only applies to shift operations and not the (real or pretended)
  bit-field operations.  Define ``SHIFT_COUNT_TRUNCATED`` to be zero on
  such machines.  Instead, add patterns to the :samp:`md` file that include
  the implied truncation of the shift instructions.

  You need not define this macro if it would always have the value of zero.

.. _target_shift_truncation_mask:

.. include:: tm.rst.in
  :start-after: [TARGET_SHIFT_TRUNCATION_MASK]
  :end-before: [TARGET_SHIFT_TRUNCATION_MASK]


.. include:: tm.rst.in
  :start-after: [TARGET_TRULY_NOOP_TRUNCATION]
  :end-before: [TARGET_TRULY_NOOP_TRUNCATION]


.. include:: tm.rst.in
  :start-after: [TARGET_MODE_REP_EXTENDED]
  :end-before: [TARGET_MODE_REP_EXTENDED]


.. include:: tm.rst.in
  :start-after: [TARGET_SETJMP_PRESERVES_NONVOLATILE_REGS_P]
  :end-before: [TARGET_SETJMP_PRESERVES_NONVOLATILE_REGS_P]


.. c:macro:: STORE_FLAG_VALUE

  A C expression describing the value returned by a comparison operator
  with an integral mode and stored by a store-flag instruction
  (:samp:`cstore{mode}4`) when the condition is true.  This description must
  apply to *all* the :samp:`cstore{mode}4` patterns and all the
  comparison operators whose results have a ``MODE_INT`` mode.

  A value of 1 or -1 means that the instruction implementing the
  comparison operator returns exactly 1 or -1 when the comparison is true
  and 0 when the comparison is false.  Otherwise, the value indicates
  which bits of the result are guaranteed to be 1 when the comparison is
  true.  This value is interpreted in the mode of the comparison
  operation, which is given by the mode of the first operand in the
  :samp:`cstore{mode}4` pattern.  Either the low bit or the sign bit of
  ``STORE_FLAG_VALUE`` be on.  Presently, only those bits are used by
  the compiler.

  If ``STORE_FLAG_VALUE`` is neither 1 or -1, the compiler will
  generate code that depends only on the specified bits.  It can also
  replace comparison operators with equivalent operations if they cause
  the required bits to be set, even if the remaining bits are undefined.
  For example, on a machine whose comparison operators return an
  ``SImode`` value and where ``STORE_FLAG_VALUE`` is defined as
  :samp:`0x80000000`, saying that just the sign bit is relevant, the
  expression

  .. code-block:: c++

    (ne:SI (and:SI x (const_int power-of-2)) (const_int 0))

  can be converted to

  .. code-block:: c++

    (ashift:SI x (const_int n))

  where :samp:`{n}` is the appropriate shift count to move the bit being
  tested into the sign bit.

  There is no way to describe a machine that always sets the low-order bit
  for a true value, but does not guarantee the value of any other bits,
  but we do not know of any machine that has such an instruction.  If you
  are trying to port GCC to such a machine, include an instruction to
  perform a logical-and of the result with 1 in the pattern for the
  comparison operators and let us know at gcc@gcc.gnu.org.

  Often, a machine will have multiple instructions that obtain a value
  from a comparison (or the condition codes).  Here are rules to guide the
  choice of value for ``STORE_FLAG_VALUE``, and hence the instructions
  to be used:

  * Use the shortest sequence that yields a valid definition for
    ``STORE_FLAG_VALUE``.  It is more efficient for the compiler to
    'normalize' the value (convert it to, e.g., 1 or 0) than for the
    comparison operators to do so because there may be opportunities to
    combine the normalization with other operations.

  * For equal-length sequences, use a value of 1 or -1, with -1 being
    slightly preferred on machines with expensive jumps and 1 preferred on
    other machines.

  * As a second choice, choose a value of :samp:`0x80000001` if instructions
    exist that set both the sign and low-order bits but do not define the
    others.

  * Otherwise, use a value of :samp:`0x80000000`.

  Many machines can produce both the value chosen for
  ``STORE_FLAG_VALUE`` and its negation in the same number of
  instructions.  On those machines, you should also define a pattern for
  those cases, e.g., one matching

  .. code-block:: c++

    (set A (neg:m (ne:m B C)))

  Some machines can also perform ``and`` or ``plus`` operations on
  condition code values with less instructions than the corresponding
  :samp:`cstore{mode}4` insn followed by ``and`` or ``plus``.  On those
  machines, define the appropriate patterns.  Use the names ``incscc``
  and ``decscc``, respectively, for the patterns which perform
  ``plus`` or ``minus`` operations on condition code values.  See
  :samp:`rs6000.md` for some examples.  The GNU Superoptimizer can be used to
  find such instruction sequences on other machines.

  If this macro is not defined, the default value, 1, is used.  You need
  not define ``STORE_FLAG_VALUE`` if the machine has no store-flag
  instructions, or if the value generated by these instructions is 1.

.. c:macro:: FLOAT_STORE_FLAG_VALUE (mode)

  A C expression that gives a nonzero ``REAL_VALUE_TYPE`` value that is
  returned when comparison operators with floating-point results are true.
  Define this macro on machines that have comparison operations that return
  floating-point values.  If there are no such operations, do not define
  this macro.

.. c:macro:: VECTOR_STORE_FLAG_VALUE (mode)

  A C expression that gives an rtx representing the nonzero true element
  for vector comparisons.  The returned rtx should be valid for the inner
  mode of :samp:`{mode}` which is guaranteed to be a vector mode.  Define
  this macro on machines that have vector comparison operations that
  return a vector result.  If there are no such operations, do not define
  this macro.  Typically, this macro is defined as ``const1_rtx`` or
  ``constm1_rtx``.  This macro may return ``NULL_RTX`` to prevent
  the compiler optimizing such vector comparison operations for the
  given mode.

.. c:macro:: CLZ_DEFINED_VALUE_AT_ZERO (mode, value)

.. c:macro:: CTZ_DEFINED_VALUE_AT_ZERO (mode, value)

  A C expression that indicates whether the architecture defines a value
  for ``clz`` or ``ctz`` with a zero operand.
  A result of ``0`` indicates the value is undefined.
  If the value is defined for only the RTL expression, the macro should
  evaluate to ``1`` ; if the value applies also to the corresponding optab
  entry (which is normally the case if it expands directly into
  the corresponding RTL), then the macro should evaluate to ``2``.
  In the cases where the value is defined, :samp:`{value}` should be set to
  this value.

  If this macro is not defined, the value of ``clz`` or
  ``ctz`` at zero is assumed to be undefined.

  This macro must be defined if the target's expansion for ``ffs``
  relies on a particular value to get correct results.  Otherwise it
  is not necessary, though it may be used to optimize some corner cases, and
  to provide a default expansion for the ``ffs`` optab.

  Note that regardless of this macro the 'definedness' of ``clz``
  and ``ctz`` at zero do *not* extend to the builtin functions
  visible to the user.  Thus one may be free to adjust the value at will
  to match the target expansion of these operations without fear of
  breaking the API.

.. c:macro:: Pmode

  An alias for the machine mode for pointers.  On most machines, define
  this to be the integer mode corresponding to the width of a hardware
  pointer; ``SImode`` on 32-bit machine or ``DImode`` on 64-bit machines.
  On some machines you must define this to be one of the partial integer
  modes, such as ``PSImode``.

  The width of ``Pmode`` must be at least as large as the value of
  ``POINTER_SIZE``.  If it is not equal, you must define the macro
  ``POINTERS_EXTEND_UNSIGNED`` to specify how pointers are extended
  to ``Pmode``.

.. c:macro:: FUNCTION_MODE

  An alias for the machine mode used for memory references to functions
  being called, in ``call`` RTL expressions.  On most CISC machines,
  where an instruction can begin at any byte address, this should be
  ``QImode``.  On most RISC machines, where all instructions have fixed
  size and alignment, this should be a mode with the same size and alignment
  as the machine instruction words - typically ``SImode`` or ``HImode``.

.. c:macro:: STDC_0_IN_SYSTEM_HEADERS

  In normal operation, the preprocessor expands ``__STDC__`` to the
  constant 1, to signify that GCC conforms to ISO Standard C.  On some
  hosts, like Solaris, the system compiler uses a different convention,
  where ``__STDC__`` is normally 0, but is 1 if the user specifies
  strict conformance to the C Standard.

  Defining ``STDC_0_IN_SYSTEM_HEADERS`` makes GNU CPP follows the host
  convention when processing system header files, but when processing user
  files ``__STDC__`` will always expand to 1.

.. include:: tm.rst.in
  :start-after: [TARGET_C_PREINCLUDE]
  :end-before: [TARGET_C_PREINCLUDE]


.. include:: tm.rst.in
  :start-after: [TARGET_CXX_IMPLICIT_EXTERN_C]
  :end-before: [TARGET_CXX_IMPLICIT_EXTERN_C]


.. c:macro:: SYSTEM_IMPLICIT_EXTERN_C

  Define this macro if the system header files do not support C++.
  This macro handles system header files by pretending that system
  header files are enclosed in :samp:`extern "C" {...}`.

.. index:: #pragma, pragma

.. c:macro:: REGISTER_TARGET_PRAGMAS ()

  Define this macro if you want to implement any target-specific pragmas.
  If defined, it is a C expression which makes a series of calls to
  ``c_register_pragma`` or ``c_register_pragma_with_expansion``
  for each pragma.  The macro may also do any
  setup required for the pragmas.

  The primary reason to define this macro is to provide compatibility with
  other compilers for the same target.  In general, we discourage
  definition of target-specific pragmas for GCC.

  If the pragma can be implemented by attributes then you should consider
  defining the target hook :samp:`TARGET_INSERT_ATTRIBUTES` as well.

  Preprocessor macros that appear on pragma lines are not expanded.  All
  :samp:`#pragma` directives that do not match any registered pragma are
  silently ignored, unless the user specifies :option:`-Wunknown-pragmas`.

.. function:: void c_register_pragma (const char *space, const char *name, void (*callback) (struct cpp_reader *))

  Each call to ``c_register_pragma`` or
  ``c_register_pragma_with_expansion`` establishes one pragma.  The
  :samp:`{callback}` routine will be called when the preprocessor encounters a
  pragma of the form

  .. code-block:: c++

    #pragma [space] name ...

  :samp:`{space}` is the case-sensitive namespace of the pragma, or
  ``NULL`` to put the pragma in the global namespace.  The callback
  routine receives :samp:`{pfile}` as its first argument, which can be passed
  on to cpplib's functions if necessary.  You can lex tokens after the
  :samp:`{name}` by calling ``pragma_lex``.  Tokens that are not read by the
  callback will be silently ignored.  The end of the line is indicated by
  a token of type ``CPP_EOF``.  Macro expansion occurs on the
  arguments of pragmas registered with
  ``c_register_pragma_with_expansion`` but not on the arguments of
  pragmas registered with ``c_register_pragma``.

  Note that the use of ``pragma_lex`` is specific to the C and C++
  compilers.  It will not work in the Java or Fortran compilers, or any
  other language compilers for that matter.  Thus if ``pragma_lex`` is going
  to be called from target-specific code, it must only be done so when
  building the C and C++ compilers.  This can be done by defining the
  variables ``c_target_objs`` and ``cxx_target_objs`` in the
  target entry in the :samp:`config.gcc` file.  These variables should name
  the target-specific, language-specific object file which contains the
  code that uses ``pragma_lex``.  Note it will also be necessary to add a
  rule to the makefile fragment pointed to by ``tmake_file`` that shows
  how to build this object file.

.. c:macro:: HANDLE_PRAGMA_PACK_WITH_EXPANSION

  Define this macro if macros should be expanded in the
  arguments of :samp:`#pragma pack`.

.. c:macro:: TARGET_DEFAULT_PACK_STRUCT

  If your target requires a structure packing default other than 0 (meaning
  the machine default), define this macro to the necessary value (in bytes).
  This must be a value that would also be valid to use with
  :samp:`#pragma pack()` (that is, a small power of two).

.. c:macro:: DOLLARS_IN_IDENTIFIERS

  Define this macro to control use of the character :samp:`$` in
  identifier names for the C family of languages.  0 means :samp:`$` is
  not allowed by default; 1 means it is allowed.  1 is the default;
  there is no need to define this macro in that case.

.. c:macro:: INSN_SETS_ARE_DELAYED (insn)

  Define this macro as a C expression that is nonzero if it is safe for the
  delay slot scheduler to place instructions in the delay slot of :samp:`{insn}`,
  even if they appear to use a resource set or clobbered in :samp:`{insn}`.
  :samp:`{insn}` is always a ``jump_insn`` or an ``insn`` ; GCC knows that
  every ``call_insn`` has this behavior.  On machines where some ``insn``
  or ``jump_insn`` is really a function call and hence has this behavior,
  you should define this macro.

  You need not define this macro if it would always return zero.

.. c:macro:: INSN_REFERENCES_ARE_DELAYED (insn)

  Define this macro as a C expression that is nonzero if it is safe for the
  delay slot scheduler to place instructions in the delay slot of :samp:`{insn}`,
  even if they appear to set or clobber a resource referenced in :samp:`{insn}`.
  :samp:`{insn}` is always a ``jump_insn`` or an ``insn``.  On machines where
  some ``insn`` or ``jump_insn`` is really a function call and its operands
  are registers whose use is actually in the subroutine it calls, you should
  define this macro.  Doing so allows the delay slot scheduler to move
  instructions which copy arguments into the argument registers into the delay
  slot of :samp:`{insn}`.

  You need not define this macro if it would always return zero.

.. c:macro:: MULTIPLE_SYMBOL_SPACES

  Define this macro as a C expression that is nonzero if, in some cases,
  global symbols from one translation unit may not be bound to undefined
  symbols in another translation unit without user intervention.  For
  instance, under Microsoft Windows symbols must be explicitly imported
  from shared libraries (DLLs).

  You need not define this macro if it would always evaluate to zero.

.. include:: tm.rst.in
  :start-after: [TARGET_MD_ASM_ADJUST]
  :end-before: [TARGET_MD_ASM_ADJUST]


.. c:macro:: MATH_LIBRARY

  Define this macro as a C string constant for the linker argument to link
  in the system math library, minus the initial :samp:`"-l"`, or
  :samp:`""` if the target does not have a
  separate math library.

  You need only define this macro if the default of :samp:`"m"` is wrong.

.. c:macro:: LIBRARY_PATH_ENV

  Define this macro as a C string constant for the environment variable that
  specifies where the linker should look for libraries.

  You need only define this macro if the default of :samp:`"LIBRARY_PATH"`
  is wrong.

.. c:macro:: TARGET_POSIX_IO

  Define this macro if the target supports the following POSIXfile
  functions, access, mkdir and  file locking with fcntl / F_SETLKW.
  Defining ``TARGET_POSIX_IO`` will enable the test coverage code
  to use file locking when exiting a program, which avoids race conditions
  if the program has forked. It will also create directories at run-time
  for cross-profiling.

.. c:macro:: MAX_CONDITIONAL_EXECUTE

  A C expression for the maximum number of instructions to execute via
  conditional execution instructions instead of a branch.  A value of
  ``BRANCH_COST`` +1 is the default.

.. c:macro:: IFCVT_MODIFY_TESTS (ce_info, true_expr, false_expr)

  Used if the target needs to perform machine-dependent modifications on the
  conditionals used for turning basic blocks into conditionally executed code.
  :samp:`{ce_info}` points to a data structure, ``struct ce_if_block``, which
  contains information about the currently processed blocks.  :samp:`{true_expr}`
  and :samp:`{false_expr}` are the tests that are used for converting the
  then-block and the else-block, respectively.  Set either :samp:`{true_expr}` or
  :samp:`{false_expr}` to a null pointer if the tests cannot be converted.

.. c:macro:: IFCVT_MODIFY_MULTIPLE_TESTS (ce_info, bb, true_expr, false_expr)

  Like ``IFCVT_MODIFY_TESTS``, but used when converting more complicated
  if-statements into conditions combined by ``and`` and ``or`` operations.
  :samp:`{bb}` contains the basic block that contains the test that is currently
  being processed and about to be turned into a condition.

.. c:macro:: IFCVT_MODIFY_INSN (ce_info, pattern, insn)

  A C expression to modify the :samp:`{PATTERN}` of an :samp:`{INSN}` that is to
  be converted to conditional execution format.  :samp:`{ce_info}` points to
  a data structure, ``struct ce_if_block``, which contains information
  about the currently processed blocks.

.. c:macro:: IFCVT_MODIFY_FINAL (ce_info)

  A C expression to perform any final machine dependent modifications in
  converting code to conditional execution.  The involved basic blocks
  can be found in the ``struct ce_if_block`` structure that is pointed
  to by :samp:`{ce_info}`.

.. c:macro:: IFCVT_MODIFY_CANCEL (ce_info)

  A C expression to cancel any machine dependent modifications in
  converting code to conditional execution.  The involved basic blocks
  can be found in the ``struct ce_if_block`` structure that is pointed
  to by :samp:`{ce_info}`.

.. c:macro:: IFCVT_MACHDEP_INIT (ce_info)

  A C expression to initialize any machine specific data for if-conversion
  of the if-block in the ``struct ce_if_block`` structure that is pointed
  to by :samp:`{ce_info}`.

.. include:: tm.rst.in
  :start-after: [TARGET_MACHINE_DEPENDENT_REORG]
  :end-before: [TARGET_MACHINE_DEPENDENT_REORG]


.. include:: tm.rst.in
  :start-after: [TARGET_INIT_BUILTINS]
  :end-before: [TARGET_INIT_BUILTINS]


.. include:: tm.rst.in
  :start-after: [TARGET_BUILTIN_DECL]
  :end-before: [TARGET_BUILTIN_DECL]


.. include:: tm.rst.in
  :start-after: [TARGET_EXPAND_BUILTIN]
  :end-before: [TARGET_EXPAND_BUILTIN]


.. include:: tm.rst.in
  :start-after: [TARGET_RESOLVE_OVERLOADED_BUILTIN]
  :end-before: [TARGET_RESOLVE_OVERLOADED_BUILTIN]


.. include:: tm.rst.in
  :start-after: [TARGET_CHECK_BUILTIN_CALL]
  :end-before: [TARGET_CHECK_BUILTIN_CALL]


.. include:: tm.rst.in
  :start-after: [TARGET_FOLD_BUILTIN]
  :end-before: [TARGET_FOLD_BUILTIN]


.. include:: tm.rst.in
  :start-after: [TARGET_GIMPLE_FOLD_BUILTIN]
  :end-before: [TARGET_GIMPLE_FOLD_BUILTIN]


.. include:: tm.rst.in
  :start-after: [TARGET_COMPARE_VERSION_PRIORITY]
  :end-before: [TARGET_COMPARE_VERSION_PRIORITY]


.. include:: tm.rst.in
  :start-after: [TARGET_GET_FUNCTION_VERSIONS_DISPATCHER]
  :end-before: [TARGET_GET_FUNCTION_VERSIONS_DISPATCHER]


.. include:: tm.rst.in
  :start-after: [TARGET_GENERATE_VERSION_DISPATCHER_BODY]
  :end-before: [TARGET_GENERATE_VERSION_DISPATCHER_BODY]


.. include:: tm.rst.in
  :start-after: [TARGET_PREDICT_DOLOOP_P]
  :end-before: [TARGET_PREDICT_DOLOOP_P]


.. include:: tm.rst.in
  :start-after: [TARGET_HAVE_COUNT_REG_DECR_P]
  :end-before: [TARGET_HAVE_COUNT_REG_DECR_P]


.. include:: tm.rst.in
  :start-after: [TARGET_DOLOOP_COST_FOR_GENERIC]
  :end-before: [TARGET_DOLOOP_COST_FOR_GENERIC]


.. include:: tm.rst.in
  :start-after: [TARGET_DOLOOP_COST_FOR_ADDRESS]
  :end-before: [TARGET_DOLOOP_COST_FOR_ADDRESS]


.. include:: tm.rst.in
  :start-after: [TARGET_CAN_USE_DOLOOP_P]
  :end-before: [TARGET_CAN_USE_DOLOOP_P]


.. include:: tm.rst.in
  :start-after: [TARGET_INVALID_WITHIN_DOLOOP]
  :end-before: [TARGET_INVALID_WITHIN_DOLOOP]


.. include:: tm.rst.in
  :start-after: [TARGET_PREFERRED_DOLOOP_MODE]
  :end-before: [TARGET_PREFERRED_DOLOOP_MODE]


.. include:: tm.rst.in
  :start-after: [TARGET_LEGITIMATE_COMBINED_INSN]
  :end-before: [TARGET_LEGITIMATE_COMBINED_INSN]


.. include:: tm.rst.in
  :start-after: [TARGET_CAN_FOLLOW_JUMP]
  :end-before: [TARGET_CAN_FOLLOW_JUMP]


.. include:: tm.rst.in
  :start-after: [TARGET_COMMUTATIVE_P]
  :end-before: [TARGET_COMMUTATIVE_P]


.. include:: tm.rst.in
  :start-after: [TARGET_ALLOCATE_INITIAL_VALUE]
  :end-before: [TARGET_ALLOCATE_INITIAL_VALUE]


.. include:: tm.rst.in
  :start-after: [TARGET_UNSPEC_MAY_TRAP_P]
  :end-before: [TARGET_UNSPEC_MAY_TRAP_P]


.. include:: tm.rst.in
  :start-after: [TARGET_SET_CURRENT_FUNCTION]
  :end-before: [TARGET_SET_CURRENT_FUNCTION]


.. c:macro:: TARGET_OBJECT_SUFFIX

  Define this macro to be a C string representing the suffix for object
  files on your target machine.  If you do not define this macro, GCC will
  use :samp:`.o` as the suffix for object files.

.. c:macro:: TARGET_EXECUTABLE_SUFFIX

  Define this macro to be a C string representing the suffix to be
  automatically added to executable files on your target machine.  If you
  do not define this macro, GCC will use the null string as the suffix for
  executable files.

.. c:macro:: COLLECT_EXPORT_LIST

  If defined, ``collect2`` will scan the individual object files
  specified on its command line and create an export list for the linker.
  Define this macro for systems like AIX, where the linker discards
  object files that are not referenced from ``main`` and uses export
  lists.

.. include:: tm.rst.in
  :start-after: [TARGET_CANNOT_MODIFY_JUMPS_P]
  :end-before: [TARGET_CANNOT_MODIFY_JUMPS_P]


.. include:: tm.rst.in
  :start-after: [TARGET_HAVE_CONDITIONAL_EXECUTION]
  :end-before: [TARGET_HAVE_CONDITIONAL_EXECUTION]


.. include:: tm.rst.in
  :start-after: [TARGET_GEN_CCMP_FIRST]
  :end-before: [TARGET_GEN_CCMP_FIRST]


.. include:: tm.rst.in
  :start-after: [TARGET_GEN_CCMP_NEXT]
  :end-before: [TARGET_GEN_CCMP_NEXT]


.. include:: tm.rst.in
  :start-after: [TARGET_GEN_MEMSET_SCRATCH_RTX]
  :end-before: [TARGET_GEN_MEMSET_SCRATCH_RTX]


.. include:: tm.rst.in
  :start-after: [TARGET_LOOP_UNROLL_ADJUST]
  :end-before: [TARGET_LOOP_UNROLL_ADJUST]


.. c:macro:: POWI_MAX_MULTS

  If defined, this macro is interpreted as a signed integer C expression
  that specifies the maximum number of floating point multiplications
  that should be emitted when expanding exponentiation by an integer
  constant inline.  When this value is defined, exponentiation requiring
  more than this number of multiplications is implemented by calling the
  system library's ``pow``, ``powf`` or ``powl`` routines.
  The default value places no upper bound on the multiplication count.

.. function:: void TARGET_EXTRA_INCLUDES (const char *sysroot, const char *iprefix, int stdinc)

  This target hook should register any extra include files for the
  target.  The parameter :samp:`{stdinc}` indicates if normal include files
  are present.  The parameter :samp:`{sysroot}` is the system root directory.
  The parameter :samp:`{iprefix}` is the prefix for the gcc directory.

.. function:: void TARGET_EXTRA_PRE_INCLUDES (const char *sysroot, const char *iprefix, int stdinc)

  This target hook should register any extra include files for the
  target before any standard headers.  The parameter :samp:`{stdinc}`
  indicates if normal include files are present.  The parameter
  :samp:`{sysroot}` is the system root directory.  The parameter
  :samp:`{iprefix}` is the prefix for the gcc directory.

.. function:: void TARGET_OPTF (char *path)

  This target hook should register special include paths for the target.
  The parameter :samp:`{path}` is the include to register.  On Darwin
  systems, this is used for Framework includes, which have semantics
  that are different from :option:`-I`.

.. function:: bool TARGET_USE_LOCAL_THUNK_ALIAS_P (tree fndecl)

  This target macro returns ``true`` if it is safe to use a local alias
  for a virtual function :samp:`{fndecl}` when constructing thunks,
  ``false`` otherwise.  By default, the macro returns ``true`` for all
  functions, if a target supports aliases (i.e. defines
  ``ASM_OUTPUT_DEF``), ``false`` otherwise,

.. c:macro:: TARGET_FORMAT_TYPES

  If defined, this macro is the name of a global variable containing
  target-specific format checking information for the :option:`-Wformat`
  option.  The default is to have no target-specific format checks.

.. c:macro:: TARGET_N_FORMAT_TYPES

  If defined, this macro is the number of entries in
  ``TARGET_FORMAT_TYPES``.

.. c:macro:: TARGET_OVERRIDES_FORMAT_ATTRIBUTES

  If defined, this macro is the name of a global variable containing
  target-specific format overrides for the :option:`-Wformat` option. The
  default is to have no target-specific format overrides. If defined,
  ``TARGET_FORMAT_TYPES`` and ``TARGET_OVERRIDES_FORMAT_ATTRIBUTES_COUNT``
  must be defined, too.

.. c:macro:: TARGET_OVERRIDES_FORMAT_ATTRIBUTES_COUNT

  If defined, this macro specifies the number of entries in
  ``TARGET_OVERRIDES_FORMAT_ATTRIBUTES``.

.. c:macro:: TARGET_OVERRIDES_FORMAT_INIT

  If defined, this macro specifies the optional initialization
  routine for target specific customizations of the system printf
  and scanf formatter settings.

.. include:: tm.rst.in
  :start-after: [TARGET_INVALID_ARG_FOR_UNPROTOTYPED_FN]
  :end-before: [TARGET_INVALID_ARG_FOR_UNPROTOTYPED_FN]


.. include:: tm.rst.in
  :start-after: [TARGET_INVALID_CONVERSION]
  :end-before: [TARGET_INVALID_CONVERSION]


.. include:: tm.rst.in
  :start-after: [TARGET_INVALID_UNARY_OP]
  :end-before: [TARGET_INVALID_UNARY_OP]


.. include:: tm.rst.in
  :start-after: [TARGET_INVALID_BINARY_OP]
  :end-before: [TARGET_INVALID_BINARY_OP]


.. include:: tm.rst.in
  :start-after: [TARGET_PROMOTED_TYPE]
  :end-before: [TARGET_PROMOTED_TYPE]


.. include:: tm.rst.in
  :start-after: [TARGET_CONVERT_TO_TYPE]
  :end-before: [TARGET_CONVERT_TO_TYPE]


.. include:: tm.rst.in
  :start-after: [TARGET_VERIFY_TYPE_CONTEXT]
  :end-before: [TARGET_VERIFY_TYPE_CONTEXT]


.. c:macro:: OBJC_JBLEN

  This macro determines the size of the objective C jump buffer for the
  NeXT runtime. By default, OBJC_JBLEN is defined to an innocuous value.

.. c:macro:: LIBGCC2_UNWIND_ATTRIBUTE

  Define this macro if any target-specific attributes need to be attached
  to the functions in :samp:`libgcc` that provide low-level support for
  call stack unwinding.  It is used in declarations in :samp:`unwind-generic.h`
  and the associated definitions of those functions.

.. include:: tm.rst.in
  :start-after: [TARGET_UPDATE_STACK_BOUNDARY]
  :end-before: [TARGET_UPDATE_STACK_BOUNDARY]


.. include:: tm.rst.in
  :start-after: [TARGET_GET_DRAP_RTX]
  :end-before: [TARGET_GET_DRAP_RTX]


.. include:: tm.rst.in
  :start-after: [TARGET_ZERO_CALL_USED_REGS]
  :end-before: [TARGET_ZERO_CALL_USED_REGS]


.. include:: tm.rst.in
  :start-after: [TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS]
  :end-before: [TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS]


.. include:: tm.rst.in
  :start-after: [TARGET_CONST_ANCHOR]
  :end-before: [TARGET_CONST_ANCHOR]


.. include:: tm.rst.in
  :start-after: [TARGET_ASAN_SHADOW_OFFSET]
  :end-before: [TARGET_ASAN_SHADOW_OFFSET]


.. include:: tm.rst.in
  :start-after: [TARGET_MEMMODEL_CHECK]
  :end-before: [TARGET_MEMMODEL_CHECK]


.. include:: tm.rst.in
  :start-after: [TARGET_ATOMIC_TEST_AND_SET_TRUEVAL]
  :end-before: [TARGET_ATOMIC_TEST_AND_SET_TRUEVAL]


.. include:: tm.rst.in
  :start-after: [TARGET_HAS_IFUNC_P]
  :end-before: [TARGET_HAS_IFUNC_P]


.. include:: tm.rst.in
  :start-after: [TARGET_IFUNC_REF_LOCAL_OK]
  :end-before: [TARGET_IFUNC_REF_LOCAL_OK]


.. include:: tm.rst.in
  :start-after: [TARGET_ATOMIC_ALIGN_FOR_MODE]
  :end-before: [TARGET_ATOMIC_ALIGN_FOR_MODE]


.. include:: tm.rst.in
  :start-after: [TARGET_ATOMIC_ASSIGN_EXPAND_FENV]
  :end-before: [TARGET_ATOMIC_ASSIGN_EXPAND_FENV]


.. include:: tm.rst.in
  :start-after: [TARGET_RECORD_OFFLOAD_SYMBOL]
  :end-before: [TARGET_RECORD_OFFLOAD_SYMBOL]


.. include:: tm.rst.in
  :start-after: [TARGET_OFFLOAD_OPTIONS]
  :end-before: [TARGET_OFFLOAD_OPTIONS]


.. c:macro:: TARGET_SUPPORTS_WIDE_INT

  On older ports, large integers are stored in ``CONST_DOUBLE`` rtl
  objects.  Newer ports define ``TARGET_SUPPORTS_WIDE_INT`` to be nonzero
  to indicate that large integers are stored in
  ``CONST_WIDE_INT`` rtl objects.  The ``CONST_WIDE_INT`` allows
  very large integer constants to be represented.  ``CONST_DOUBLE``
  is limited to twice the size of the host's ``HOST_WIDE_INT``
  representation.

  Converting a port mostly requires looking for the places where
  ``CONST_DOUBLE`` s are used with ``VOIDmode`` and replacing that
  code with code that accesses ``CONST_WIDE_INT`` s.  :samp:`"grep -i
  const_double"` at the port level gets you to 95% of the changes that
  need to be made.  There are a few places that require a deeper look.

  * There is no equivalent to ``hval`` and ``lval`` for
    ``CONST_WIDE_INT`` s.  This would be difficult to express in the md
    language since there are a variable number of elements.

    Most ports only check that ``hval`` is either 0 or -1 to see if the
    value is small.  As mentioned above, this will no longer be necessary
    since small constants are always ``CONST_INT``.  Of course there
    are still a few exceptions, the alpha's constraint used by the zap
    instruction certainly requires careful examination by C code.
    However, all the current code does is pass the hval and lval to C
    code, so evolving the c code to look at the ``CONST_WIDE_INT`` is
    not really a large change.

  * Because there is no standard template that ports use to materialize
    constants, there is likely to be some futzing that is unique to each
    port in this code.

  * The rtx costs may have to be adjusted to properly account for larger
    constants that are represented as ``CONST_WIDE_INT``.

  All and all it does not take long to convert ports that the
  maintainer is familiar with.

.. include:: tm.rst.in
  :start-after: [TARGET_HAVE_SPECULATION_SAFE_VALUE]
  :end-before: [TARGET_HAVE_SPECULATION_SAFE_VALUE]


.. include:: tm.rst.in
  :start-after: [TARGET_SPECULATION_SAFE_VALUE]
  :end-before: [TARGET_SPECULATION_SAFE_VALUE]


.. include:: tm.rst.in
  :start-after: [TARGET_RUN_TARGET_SELFTESTS]
  :end-before: [TARGET_RUN_TARGET_SELFTESTS]


.. include:: tm.rst.in
  :start-after: [TARGET_MEMTAG_CAN_TAG_ADDRESSES]
  :end-before: [TARGET_MEMTAG_CAN_TAG_ADDRESSES]


.. include:: tm.rst.in
  :start-after: [TARGET_MEMTAG_TAG_SIZE]
  :end-before: [TARGET_MEMTAG_TAG_SIZE]


.. include:: tm.rst.in
  :start-after: [TARGET_MEMTAG_GRANULE_SIZE]
  :end-before: [TARGET_MEMTAG_GRANULE_SIZE]


.. include:: tm.rst.in
  :start-after: [TARGET_MEMTAG_INSERT_RANDOM_TAG]
  :end-before: [TARGET_MEMTAG_INSERT_RANDOM_TAG]


.. include:: tm.rst.in
  :start-after: [TARGET_MEMTAG_ADD_TAG]
  :end-before: [TARGET_MEMTAG_ADD_TAG]


.. include:: tm.rst.in
  :start-after: [TARGET_MEMTAG_SET_TAG]
  :end-before: [TARGET_MEMTAG_SET_TAG]


.. include:: tm.rst.in
  :start-after: [TARGET_MEMTAG_EXTRACT_TAG]
  :end-before: [TARGET_MEMTAG_EXTRACT_TAG]


.. include:: tm.rst.in
  :start-after: [TARGET_MEMTAG_UNTAGGED_POINTER]
  :end-before: [TARGET_MEMTAG_UNTAGGED_POINTER]


.. include:: tm.rst.in
  :start-after: [TARGET_GCOV_TYPE_SIZE]
  :end-before: [TARGET_GCOV_TYPE_SIZE]


.. include:: tm.rst.in
  :start-after: [TARGET_HAVE_SHADOW_CALL_STACK]
  :end-before: [TARGET_HAVE_SHADOW_CALL_STACK]
