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

.. function:: unsigned int TARGET_CASE_VALUES_THRESHOLD (void)

  .. hook-start:TARGET_CASE_VALUES_THRESHOLD

  This function return the smallest number of different values for which it
  is best to use a jump-table instead of a tree of conditional branches.
  The default is four for machines with a ``casesi`` instruction and
  five otherwise.  This is best for most machines.

.. hook-end

.. c:macro:: WORD_REGISTER_OPERATIONS

  Define this macro to 1 if operations between registers with integral mode
  smaller than a word are always performed on the entire register.  To be
  more explicit, if you start with a pair of ``word_mode`` registers with
  known values and you do a subword, for example ``QImode``, addition on
  the low part of the registers, then the compiler may consider that the
  result has a known value in ``word_mode`` too if the macro is defined
  to 1.  Most RISC machines have this property and most CISC machines do not.

.. function:: unsigned int TARGET_MIN_ARITHMETIC_PRECISION (void)

  .. hook-start:TARGET_MIN_ARITHMETIC_PRECISION

  On some RISC architectures with 64-bit registers, the processor also
  maintains 32-bit condition codes that make it possible to do real 32-bit
  arithmetic, although the operations are performed on the full registers.

  On such architectures, defining this hook to 32 tells the compiler to try
  using 32-bit arithmetical operations setting the condition codes instead
  of doing full 64-bit arithmetic.

  More generally, define this hook on RISC architectures if you want the
  compiler to try using arithmetical operations setting the condition codes
  with a precision lower than the word precision.

  You need not define this hook if ``WORD_REGISTER_OPERATIONS`` is not
  defined to 1.

.. hook-end

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

.. function:: unsigned int TARGET_MIN_DIVISIONS_FOR_RECIP_MUL (machine_mode mode)

  .. hook-start:TARGET_MIN_DIVISIONS_FOR_RECIP_MUL

  When :option:`-ffast-math` is in effect, GCC tries to optimize
  divisions by the same divisor, by turning them into multiplications by
  the reciprocal.  This target hook specifies the minimum number of divisions
  that should be there for GCC to perform the optimization for a variable
  of mode :samp:`{mode}`.  The default implementation returns 3 if the machine
  has an instruction for the division, and 2 if it does not.

.. hook-end

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

.. function:: unsigned HOST_WIDE_INT TARGET_SHIFT_TRUNCATION_MASK (machine_mode mode)

  .. hook-start:TARGET_SHIFT_TRUNCATION_MASK

  This function describes how the standard shift patterns for :samp:`{mode}`
  deal with shifts by negative amounts or by more than the width of the mode.
  See :ref:`shift-patterns`.

  On many machines, the shift patterns will apply a mask :samp:`{m}` to the
  shift count, meaning that a fixed-width shift of :samp:`{x}` by :samp:`{y}` is
  equivalent to an arbitrary-width shift of :samp:`{x}` by :samp:`{y & m}`.  If
  this is true for mode :samp:`{mode}`, the function should return :samp:`{m}`,
  otherwise it should return 0.  A return value of 0 indicates that no
  particular behavior is guaranteed.

  Note that, unlike ``SHIFT_COUNT_TRUNCATED``, this function does
  *not* apply to general shift rtxes; it applies only to instructions
  that are generated by the named shift patterns.

  The default implementation of this function returns
  ``GET_MODE_BITSIZE (mode) - 1`` if ``SHIFT_COUNT_TRUNCATED``
  and 0 otherwise.  This definition is always safe, but if
  ``SHIFT_COUNT_TRUNCATED`` is false, and some shift patterns
  nevertheless truncate the shift count, you may get better code
  by overriding it.

.. hook-end

.. function:: bool TARGET_TRULY_NOOP_TRUNCATION (poly_uint64 outprec, poly_uint64 inprec)

  .. hook-start:TARGET_TRULY_NOOP_TRUNCATION

  This hook returns true if it is safe to 'convert' a value of
  :samp:`{inprec}` bits to one of :samp:`{outprec}` bits (where :samp:`{outprec}` is
  smaller than :samp:`{inprec}`) by merely operating on it as if it had only
  :samp:`{outprec}` bits.  The default returns true unconditionally, which
  is correct for most machines.  When ``TARGET_TRULY_NOOP_TRUNCATION``
  returns false, the machine description should provide a ``trunc``
  optab to specify the RTL that performs the required truncation.

  If ``TARGET_MODES_TIEABLE_P`` returns false for a pair of modes,
  suboptimal code can result if this hook returns true for the corresponding
  mode sizes.  Making this hook return false in such cases may improve things.

.. hook-end

.. function:: int TARGET_MODE_REP_EXTENDED (scalar_int_mode mode, scalar_int_mode rep_mode)

  .. hook-start:TARGET_MODE_REP_EXTENDED

  The representation of an integral mode can be such that the values
  are always extended to a wider integral mode.  Return
  ``SIGN_EXTEND`` if values of :samp:`{mode}` are represented in
  sign-extended form to :samp:`{rep_mode}`.  Return ``UNKNOWN``
  otherwise.  (Currently, none of the targets use zero-extended
  representation this way so unlike ``LOAD_EXTEND_OP``,
  ``TARGET_MODE_REP_EXTENDED`` is expected to return either
  ``SIGN_EXTEND`` or ``UNKNOWN``.  Also no target extends
  :samp:`{mode}` to :samp:`{rep_mode}` so that :samp:`{rep_mode}` is not the next
  widest integral mode and currently we take advantage of this fact.)

  Similarly to ``LOAD_EXTEND_OP`` you may return a non- ``UNKNOWN``
  value even if the extension is not performed on certain hard registers
  as long as for the ``REGNO_REG_CLASS`` of these hard registers
  ``TARGET_CAN_CHANGE_MODE_CLASS`` returns false.

  Note that ``TARGET_MODE_REP_EXTENDED`` and ``LOAD_EXTEND_OP``
  describe two related properties.  If you define
  ``TARGET_MODE_REP_EXTENDED (mode, word_mode)`` you probably also want
  to define ``LOAD_EXTEND_OP (mode)`` to return the same type of
  extension.

  In order to enforce the representation of ``mode``,
  ``TARGET_TRULY_NOOP_TRUNCATION`` should return false when truncating to
  ``mode``.

.. hook-end

.. function:: bool TARGET_SETJMP_PRESERVES_NONVOLATILE_REGS_P (void)

  .. hook-start:TARGET_SETJMP_PRESERVES_NONVOLATILE_REGS_P

  On some targets, it is assumed that the compiler will spill all pseudos
  that are live across a call to ``setjmp``, while other targets treat
  ``setjmp`` calls as normal function calls.

  This hook returns false if ``setjmp`` calls do not preserve all
  non-volatile registers so that gcc that must spill all pseudos that are
  live across ``setjmp`` calls.  Define this to return true if the
  target does not need to spill all pseudos live across ``setjmp`` calls.
  The default implementation conservatively assumes all pseudos must be
  spilled across ``setjmp`` calls.

.. hook-end

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

.. function:: const char * TARGET_C_PREINCLUDE (void)

  .. hook-start:TARGET_C_PREINCLUDE

  Define this hook to return the name of a header file to be included at
  the start of all compilations, as if it had been included with
  ``#include <file>``.  If this hook returns ``NULL``, or is
  not defined, or the header is not found, or if the user specifies
  :option:`-ffreestanding` or :option:`-nostdinc`, no header is included.

  This hook can be used together with a header provided by the system C
  library to implement ISO C requirements for certain macros to be
  predefined that describe properties of the whole implementation rather
  than just the compiler.

.. hook-end

.. function:: bool TARGET_CXX_IMPLICIT_EXTERN_C (const char*)

  .. hook-start:TARGET_CXX_IMPLICIT_EXTERN_C

  Define this hook to add target-specific C++ implicit extern C functions.
  If this function returns true for the name of a file-scope function, that
  function implicitly gets extern "C" linkage rather than whatever language
  linkage the declaration would normally have.  An example of such function
  is WinMain on Win32 targets.

.. hook-end

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

.. function:: rtx_insn * TARGET_MD_ASM_ADJUST (vec<rtx>& outputs, vec<rtx>& inputs, vec<machine_mode>& input_modes, vec<const char *>& constraints, vec<rtx>& clobbers, HARD_REG_SET& clobbered_regs, location_t loc)

  .. hook-start:TARGET_MD_ASM_ADJUST

  This target hook may add :dfn:`clobbers` to :samp:`{clobbers}` and
  :samp:`{clobbered_regs}` for any hard regs the port wishes to automatically
  clobber for an asm.  The :samp:`{outputs}` and :samp:`{inputs}` may be inspected
  to avoid clobbering a register that is already used by the asm.  :samp:`{loc}`
  is the source location of the asm.

  It may modify the :samp:`{outputs}`, :samp:`{inputs}`, :samp:`{input_modes}`, and
  :samp:`{constraints}` as necessary for other pre-processing.  In this case the
  return value is a sequence of insns to emit after the asm.  Note that
  changes to :samp:`{inputs}` must be accompanied by the corresponding changes
  to :samp:`{input_modes}`.

.. hook-end

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

.. function:: void TARGET_MACHINE_DEPENDENT_REORG (void)

  .. hook-start:TARGET_MACHINE_DEPENDENT_REORG

  If non-null, this hook performs a target-specific pass over the
  instruction stream.  The compiler will run it at all optimization levels,
  just before the point at which it normally does delayed-branch scheduling.

  The exact purpose of the hook varies from target to target.  Some use
  it to do transformations that are necessary for correctness, such as
  laying out in-function constant pools or avoiding hardware hazards.
  Others use it as an opportunity to do some machine-dependent optimizations.

  You need not implement the hook if it has nothing to do.  The default
  definition is null.

.. hook-end

.. function:: void TARGET_INIT_BUILTINS (void)

  .. hook-start:TARGET_INIT_BUILTINS

  Define this hook if you have any machine-specific built-in functions
  that need to be defined.  It should be a function that performs the
  necessary setup.

  Machine specific built-in functions can be useful to expand special machine
  instructions that would otherwise not normally be generated because
  they have no equivalent in the source language (for example, SIMD vector
  instructions or prefetch instructions).

  To create a built-in function, call the function
  ``lang_hooks.builtin_function``
  which is defined by the language front end.  You can use any type nodes set
  up by ``build_common_tree_nodes`` ;
  only language front ends that use those two functions will call
  :samp:`TARGET_INIT_BUILTINS`.

.. hook-end

.. function:: tree TARGET_BUILTIN_DECL (unsigned code, bool initialize_p)

  .. hook-start:TARGET_BUILTIN_DECL

  Define this hook if you have any machine-specific built-in functions
  that need to be defined.  It should be a function that returns the
  builtin function declaration for the builtin function code :samp:`{code}`.
  If there is no such builtin and it cannot be initialized at this time
  if :samp:`{initialize_p}` is true the function should return ``NULL_TREE``.
  If :samp:`{code}` is out of range the function should return
  ``error_mark_node``.

.. hook-end

.. function:: rtx TARGET_EXPAND_BUILTIN (tree exp, rtx target, rtx subtarget, machine_mode mode, int ignore)

  .. hook-start:TARGET_EXPAND_BUILTIN

  Expand a call to a machine specific built-in function that was set up by
  :samp:`TARGET_INIT_BUILTINS`.  :samp:`{exp}` is the expression for the
  function call; the result should go to :samp:`{target}` if that is
  convenient, and have mode :samp:`{mode}` if that is convenient.
  :samp:`{subtarget}` may be used as the target for computing one of
  :samp:`{exp}` 's operands.  :samp:`{ignore}` is nonzero if the value is to be
  ignored.  This function should return the result of the call to the
  built-in function.

.. hook-end

.. function:: tree TARGET_RESOLVE_OVERLOADED_BUILTIN (unsigned int loc, tree fndecl, void *arglist)

  .. hook-start:TARGET_RESOLVE_OVERLOADED_BUILTIN

  Select a replacement for a machine specific built-in function that
  was set up by :samp:`TARGET_INIT_BUILTINS`.  This is done
  *before* regular type checking, and so allows the target to
  implement a crude form of function overloading.  :samp:`{fndecl}` is the
  declaration of the built-in function.  :samp:`{arglist}` is the list of
  arguments passed to the built-in function.  The result is a
  complete expression that implements the operation, usually
  another ``CALL_EXPR``.
  :samp:`{arglist}` really has type :samp:`VEC(tree,gc)*`

.. hook-end

.. function:: bool TARGET_CHECK_BUILTIN_CALL (location_t loc, vec<location_t> arg_loc, tree fndecl, tree orig_fndecl, unsigned int nargs, tree *args)

  .. hook-start:TARGET_CHECK_BUILTIN_CALL

  Perform semantic checking on a call to a machine-specific built-in
  function after its arguments have been constrained to the function
  signature.  Return true if the call is valid, otherwise report an error
  and return false.

  This hook is called after ``TARGET_RESOLVE_OVERLOADED_BUILTIN``.
  The call was originally to built-in function :samp:`{orig_fndecl}`,
  but after the optional ``TARGET_RESOLVE_OVERLOADED_BUILTIN``
  step is now to built-in function :samp:`{fndecl}`.  :samp:`{loc}` is the
  location of the call and :samp:`{args}` is an array of function arguments,
  of which there are :samp:`{nargs}`.  :samp:`{arg_loc}` specifies the location
  of each argument.

.. hook-end

.. function:: tree TARGET_FOLD_BUILTIN (tree fndecl, int n_args, tree *argp, bool ignore)

  .. hook-start:TARGET_FOLD_BUILTIN

  Fold a call to a machine specific built-in function that was set up by
  :samp:`TARGET_INIT_BUILTINS`.  :samp:`{fndecl}` is the declaration of the
  built-in function.  :samp:`{n_args}` is the number of arguments passed to
  the function; the arguments themselves are pointed to by :samp:`{argp}`.
  The result is another tree, valid for both GIMPLE and GENERIC,
  containing a simplified expression for the call's result.  If
  :samp:`{ignore}` is true the value will be ignored.

.. hook-end

.. function:: bool TARGET_GIMPLE_FOLD_BUILTIN (gimple_stmt_iterator *gsi)

  .. hook-start:TARGET_GIMPLE_FOLD_BUILTIN

  Fold a call to a machine specific built-in function that was set up
  by :samp:`TARGET_INIT_BUILTINS`.  :samp:`{gsi}` points to the gimple
  statement holding the function call.  Returns true if any change
  was made to the GIMPLE stream.

.. hook-end

.. function:: int TARGET_COMPARE_VERSION_PRIORITY (tree decl1, tree decl2)

  .. hook-start:TARGET_COMPARE_VERSION_PRIORITY

  This hook is used to compare the target attributes in two functions to
  determine which function's features get higher priority.  This is used
  during function multi-versioning to figure out the order in which two
  versions must be dispatched.  A function version with a higher priority
  is checked for dispatching earlier.  :samp:`{decl1}` and :samp:`{decl2}` are
  the two function decls that will be compared.

.. hook-end

.. function:: tree TARGET_GET_FUNCTION_VERSIONS_DISPATCHER (void *decl)

  .. hook-start:TARGET_GET_FUNCTION_VERSIONS_DISPATCHER

  This hook is used to get the dispatcher function for a set of function
  versions.  The dispatcher function is called to invoke the right function
  version at run-time. :samp:`{decl}` is one version from a set of semantically
  identical versions.

.. hook-end

.. function:: tree TARGET_GENERATE_VERSION_DISPATCHER_BODY (void *arg)

  .. hook-start:TARGET_GENERATE_VERSION_DISPATCHER_BODY

  This hook is used to generate the dispatcher logic to invoke the right
  function version at run-time for a given set of function versions.
  :samp:`{arg}` points to the callgraph node of the dispatcher function whose
  body must be generated.

.. hook-end

.. function:: bool TARGET_PREDICT_DOLOOP_P (class loop *loop)

  .. hook-start:TARGET_PREDICT_DOLOOP_P

  Return true if we can predict it is possible to use a low-overhead loop
  for a particular loop.  The parameter :samp:`{loop}` is a pointer to the loop.
  This target hook is required only when the target supports low-overhead
  loops, and will help ivopts to make some decisions.
  The default version of this hook returns false.

.. hook-end

.. c:var:: bool TARGET_HAVE_COUNT_REG_DECR_P

  .. hook-start:TARGET_HAVE_COUNT_REG_DECR_P

  Return true if the target supports hardware count register for decrement
  and branch.
  The default value is false.

.. hook-end

.. c:var:: int64_t TARGET_DOLOOP_COST_FOR_GENERIC

  .. hook-start:TARGET_DOLOOP_COST_FOR_GENERIC

  One IV candidate dedicated for doloop is introduced in IVOPTs, we can
  calculate the computation cost of adopting it to any generic IV use by
  function get_computation_cost as before.  But for targets which have
  hardware count register support for decrement and branch, it may have to
  move IV value from hardware count register to general purpose register
  while doloop IV candidate is used for generic IV uses.  It probably takes
  expensive penalty.  This hook allows target owners to define the cost for
  this especially for generic IV uses.
  The default value is zero.

.. hook-end

.. c:var:: int64_t TARGET_DOLOOP_COST_FOR_ADDRESS

  .. hook-start:TARGET_DOLOOP_COST_FOR_ADDRESS

  One IV candidate dedicated for doloop is introduced in IVOPTs, we can
  calculate the computation cost of adopting it to any address IV use by
  function get_computation_cost as before.  But for targets which have
  hardware count register support for decrement and branch, it may have to
  move IV value from hardware count register to general purpose register
  while doloop IV candidate is used for address IV uses.  It probably takes
  expensive penalty.  This hook allows target owners to define the cost for
  this escpecially for address IV uses.
  The default value is zero.

.. hook-end

.. function:: bool TARGET_CAN_USE_DOLOOP_P (const widest_int &iterations, const widest_int &iterations_max, unsigned int loop_depth, bool entered_at_top)

  .. hook-start:TARGET_CAN_USE_DOLOOP_P

  Return true if it is possible to use low-overhead loops (``doloop_end``
  and ``doloop_begin``) for a particular loop.  :samp:`{iterations}` gives the
  exact number of iterations, or 0 if not known.  :samp:`{iterations_max}` gives
  the maximum number of iterations, or 0 if not known.  :samp:`{loop_depth}` is
  the nesting depth of the loop, with 1 for innermost loops, 2 for loops that
  contain innermost loops, and so on.  :samp:`{entered_at_top}` is true if the
  loop is only entered from the top.

  This hook is only used if ``doloop_end`` is available.  The default
  implementation returns true.  You can use ``can_use_doloop_if_innermost``
  if the loop must be the innermost, and if there are no other restrictions.

.. hook-end

.. function:: const char * TARGET_INVALID_WITHIN_DOLOOP (const rtx_insn *insn)

  .. hook-start:TARGET_INVALID_WITHIN_DOLOOP

  Take an instruction in :samp:`{insn}` and return NULL if it is valid within a
  low-overhead loop, otherwise return a string explaining why doloop
  could not be applied.

  Many targets use special registers for low-overhead looping. For any
  instruction that clobbers these this function should return a string indicating
  the reason why the doloop could not be applied.
  By default, the RTL loop optimizer does not use a present doloop pattern for
  loops containing function calls or branch on table instructions.

.. hook-end

.. function:: machine_mode TARGET_PREFERRED_DOLOOP_MODE (machine_mode mode)

  .. hook-start:TARGET_PREFERRED_DOLOOP_MODE

  This hook takes a :samp:`{mode}` for a doloop IV, where ``mode`` is the
  original mode for the operation.  If the target prefers an alternate
  ``mode`` for the operation, then this hook should return that mode;
  otherwise the original ``mode`` should be returned.  For example, on a
  64-bit target, ``DImode`` might be preferred over ``SImode``.  Both the
  original and the returned modes should be ``MODE_INT``.

.. hook-end

.. function:: bool TARGET_LEGITIMATE_COMBINED_INSN (rtx_insn *insn)

  .. hook-start:TARGET_LEGITIMATE_COMBINED_INSN

  Take an instruction in :samp:`{insn}` and return ``false`` if the instruction
  is not appropriate as a combination of two or more instructions.  The
  default is to accept all instructions.

.. hook-end

.. function:: bool TARGET_CAN_FOLLOW_JUMP (const rtx_insn *follower, const rtx_insn *followee)

  .. hook-start:TARGET_CAN_FOLLOW_JUMP

  FOLLOWER and FOLLOWEE are JUMP_INSN instructions;
  return true if FOLLOWER may be modified to follow FOLLOWEE;
  false, if it can't.
  For example, on some targets, certain kinds of branches can't be made to
  follow through a hot/cold partitioning.

.. hook-end

.. function:: bool TARGET_COMMUTATIVE_P (const_rtx x, int outer_code)

  .. hook-start:TARGET_COMMUTATIVE_P

  This target hook returns ``true`` if :samp:`{x}` is considered to be commutative.
  Usually, this is just COMMUTATIVE_P (:samp:`{x}`), but the HP PA doesn't consider
  PLUS to be commutative inside a MEM.  :samp:`{outer_code}` is the rtx code
  of the enclosing rtl, if known, otherwise it is UNKNOWN.

.. hook-end

.. function:: rtx TARGET_ALLOCATE_INITIAL_VALUE (rtx hard_reg)

  .. hook-start:TARGET_ALLOCATE_INITIAL_VALUE

  When the initial value of a hard register has been copied in a pseudo
  register, it is often not necessary to actually allocate another register
  to this pseudo register, because the original hard register or a stack slot
  it has been saved into can be used.  ``TARGET_ALLOCATE_INITIAL_VALUE``
  is called at the start of register allocation once for each hard register
  that had its initial value copied by using
  ``get_func_hard_reg_initial_val`` or ``get_hard_reg_initial_val``.
  Possible values are ``NULL_RTX``, if you don't want
  to do any special allocation, a ``REG`` rtx---that would typically be
  the hard register itself, if it is known not to be clobbered---or a
  ``MEM``.
  If you are returning a ``MEM``, this is only a hint for the allocator;
  it might decide to use another register anyways.
  You may use ``current_function_is_leaf`` or
  ``REG_N_SETS`` in the hook to determine if the hard
  register in question will not be clobbered.
  The default value of this hook is ``NULL``, which disables any special
  allocation.

.. hook-end

.. function:: int TARGET_UNSPEC_MAY_TRAP_P (const_rtx x, unsigned flags)

  .. hook-start:TARGET_UNSPEC_MAY_TRAP_P

  This target hook returns nonzero if :samp:`{x}`, an ``unspec`` or
  ``unspec_volatile`` operation, might cause a trap.  Targets can use
  this hook to enhance precision of analysis for ``unspec`` and
  ``unspec_volatile`` operations.  You may call ``may_trap_p_1``
  to analyze inner elements of :samp:`{x}` in which case :samp:`{flags}` should be
  passed along.

.. hook-end

.. function:: void TARGET_SET_CURRENT_FUNCTION (tree decl)

  .. hook-start:TARGET_SET_CURRENT_FUNCTION

  The compiler invokes this hook whenever it changes its current function
  context (``cfun``).  You can define this function if
  the back end needs to perform any initialization or reset actions on a
  per-function basis.  For example, it may be used to implement function
  attributes that affect register usage or code generation patterns.
  The argument :samp:`{decl}` is the declaration for the new function context,
  and may be null to indicate that the compiler has left a function context
  and is returning to processing at the top level.
  The default hook function does nothing.

  GCC sets ``cfun`` to a dummy function context during initialization of
  some parts of the back end.  The hook function is not invoked in this
  situation; you need not worry about the hook being invoked recursively,
  or when the back end is in a partially-initialized state.
  ``cfun`` might be ``NULL`` to indicate processing at top level,
  outside of any function scope.

.. hook-end

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

.. function:: bool TARGET_CANNOT_MODIFY_JUMPS_P (void)

  .. hook-start:TARGET_CANNOT_MODIFY_JUMPS_P

  This target hook returns ``true`` past the point in which new jump
  instructions could be created.  On machines that require a register for
  every jump such as the SHmedia ISA of SH5, this point would typically be
  reload, so this target hook should be defined to a function such as:

  .. code-block:: c++

    static bool
    cannot_modify_jumps_past_reload_p ()
    {
      return (reload_completed || reload_in_progress);
    }

.. hook-end

.. function:: bool TARGET_HAVE_CONDITIONAL_EXECUTION (void)

  .. hook-start:TARGET_HAVE_CONDITIONAL_EXECUTION

  This target hook returns true if the target supports conditional execution.
  This target hook is required only when the target has several different
  modes and they have different conditional execution capability, such as ARM.

.. hook-end

.. function:: rtx TARGET_GEN_CCMP_FIRST (rtx_insn **prep_seq, rtx_insn **gen_seq, int code, tree op0, tree op1)

  .. hook-start:TARGET_GEN_CCMP_FIRST

  This function prepares to emit a comparison insn for the first compare in a
  sequence of conditional comparisions.  It returns an appropriate comparison
  with ``CC`` for passing to ``gen_ccmp_next`` or ``cbranch_optab``.
  The insns to prepare the compare are saved in :samp:`{prep_seq}` and the compare
  insns are saved in :samp:`{gen_seq}`.  They will be emitted when all the
  compares in the conditional comparision are generated without error.
  :samp:`{code}` is the ``rtx_code`` of the compare for :samp:`{op0}` and :samp:`{op1}`.

.. hook-end

.. function:: rtx TARGET_GEN_CCMP_NEXT (rtx_insn **prep_seq, rtx_insn **gen_seq, rtx prev, int cmp_code, tree op0, tree op1, int bit_code)

  .. hook-start:TARGET_GEN_CCMP_NEXT

  This function prepares to emit a conditional comparison within a sequence
  of conditional comparisons.  It returns an appropriate comparison with
  ``CC`` for passing to ``gen_ccmp_next`` or ``cbranch_optab``.
  The insns to prepare the compare are saved in :samp:`{prep_seq}` and the compare
  insns are saved in :samp:`{gen_seq}`.  They will be emitted when all the
  compares in the conditional comparision are generated without error.  The
  :samp:`{prev}` expression is the result of a prior call to ``gen_ccmp_first``
  or ``gen_ccmp_next``.  It may return ``NULL`` if the combination of
  :samp:`{prev}` and this comparison is not supported, otherwise the result must
  be appropriate for passing to ``gen_ccmp_next`` or ``cbranch_optab``.
  :samp:`{code}` is the ``rtx_code`` of the compare for :samp:`{op0}` and :samp:`{op1}`.
  :samp:`{bit_code}` is ``AND`` or ``IOR``, which is the op on the compares.

.. hook-end

.. function:: rtx TARGET_GEN_MEMSET_SCRATCH_RTX (machine_mode mode)

  .. hook-start:TARGET_GEN_MEMSET_SCRATCH_RTX

  This hook should return an rtx for a scratch register in :samp:`{mode}` to
  be used when expanding memset calls.  The backend can use a hard scratch
  register to avoid stack realignment when expanding memset.  The default
  is ``gen_reg_rtx``.

.. hook-end

.. function:: unsigned TARGET_LOOP_UNROLL_ADJUST (unsigned nunroll, class loop *loop)

  .. hook-start:TARGET_LOOP_UNROLL_ADJUST

  This target hook returns a new value for the number of times :samp:`{loop}`
  should be unrolled. The parameter :samp:`{nunroll}` is the number of times
  the loop is to be unrolled. The parameter :samp:`{loop}` is a pointer to
  the loop, which is going to be checked for unrolling. This target hook
  is required only when the target has special constraints like maximum
  number of memory accesses.

.. hook-end

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

.. function:: const char * TARGET_INVALID_ARG_FOR_UNPROTOTYPED_FN (const_tree typelist, const_tree funcdecl, const_tree val)

  .. hook-start:TARGET_INVALID_ARG_FOR_UNPROTOTYPED_FN

  If defined, this macro returns the diagnostic message when it is
  illegal to pass argument :samp:`{val}` to function :samp:`{funcdecl}`
  with prototype :samp:`{typelist}`.

.. hook-end

.. function:: const char * TARGET_INVALID_CONVERSION (const_tree fromtype, const_tree totype)

  .. hook-start:TARGET_INVALID_CONVERSION

  If defined, this macro returns the diagnostic message when it is
  invalid to convert from :samp:`{fromtype}` to :samp:`{totype}`, or ``NULL``
  if validity should be determined by the front end.

.. hook-end

.. function:: const char * TARGET_INVALID_UNARY_OP (int op, const_tree type)

  .. hook-start:TARGET_INVALID_UNARY_OP

  If defined, this macro returns the diagnostic message when it is
  invalid to apply operation :samp:`{op}` (where unary plus is denoted by
  ``CONVERT_EXPR``) to an operand of type :samp:`{type}`, or ``NULL``
  if validity should be determined by the front end.

.. hook-end

.. function:: const char * TARGET_INVALID_BINARY_OP (int op, const_tree type1, const_tree type2)

  .. hook-start:TARGET_INVALID_BINARY_OP

  If defined, this macro returns the diagnostic message when it is
  invalid to apply operation :samp:`{op}` to operands of types :samp:`{type1}`
  and :samp:`{type2}`, or ``NULL`` if validity should be determined by
  the front end.

.. hook-end

.. function:: tree TARGET_PROMOTED_TYPE (const_tree type)

  .. hook-start:TARGET_PROMOTED_TYPE

  If defined, this target hook returns the type to which values of
  :samp:`{type}` should be promoted when they appear in expressions,
  analogous to the integer promotions, or ``NULL_TREE`` to use the
  front end's normal promotion rules.  This hook is useful when there are
  target-specific types with special promotion rules.
  This is currently used only by the C and C++ front ends.

.. hook-end

.. function:: tree TARGET_CONVERT_TO_TYPE (tree type, tree expr)

  .. hook-start:TARGET_CONVERT_TO_TYPE

  If defined, this hook returns the result of converting :samp:`{expr}` to
  :samp:`{type}`.  It should return the converted expression,
  or ``NULL_TREE`` to apply the front end's normal conversion rules.
  This hook is useful when there are target-specific types with special
  conversion rules.
  This is currently used only by the C and C++ front ends.

.. hook-end

.. function:: bool TARGET_VERIFY_TYPE_CONTEXT (location_t loc, type_context_kind context, const_tree type, bool silent_p)

  .. hook-start:TARGET_VERIFY_TYPE_CONTEXT

  If defined, this hook returns false if there is a target-specific reason
  why type :samp:`{type}` cannot be used in the source language context described
  by :samp:`{context}`.  When :samp:`{silent_p}` is false, the hook also reports an
  error against :samp:`{loc}` for invalid uses of :samp:`{type}`.

  Calls to this hook should be made through the global function
  ``verify_type_context``, which makes the :samp:`{silent_p}` parameter
  default to false and also handles ``error_mark_node``.

  The default implementation always returns true.

.. hook-end

.. c:macro:: OBJC_JBLEN

  This macro determines the size of the objective C jump buffer for the
  NeXT runtime. By default, OBJC_JBLEN is defined to an innocuous value.

.. c:macro:: LIBGCC2_UNWIND_ATTRIBUTE

  Define this macro if any target-specific attributes need to be attached
  to the functions in :samp:`libgcc` that provide low-level support for
  call stack unwinding.  It is used in declarations in :samp:`unwind-generic.h`
  and the associated definitions of those functions.

.. function:: void TARGET_UPDATE_STACK_BOUNDARY (void)

  .. hook-start:TARGET_UPDATE_STACK_BOUNDARY

  Define this macro to update the current function stack boundary if
  necessary.

.. hook-end

.. function:: rtx TARGET_GET_DRAP_RTX (void)

  .. hook-start:TARGET_GET_DRAP_RTX

  This hook should return an rtx for Dynamic Realign Argument Pointer (DRAP) if a
  different argument pointer register is needed to access the function's
  argument list due to stack realignment.  Return ``NULL`` if no DRAP
  is needed.

.. hook-end

.. function:: HARD_REG_SET TARGET_ZERO_CALL_USED_REGS (HARD_REG_SET selected_regs)

  .. hook-start:TARGET_ZERO_CALL_USED_REGS

  This target hook emits instructions to zero the subset of :samp:`{selected_regs}`
  that could conceivably contain values that are useful to an attacker.
  Return the set of registers that were actually cleared.

  For most targets, the returned set of registers is a subset of
  :samp:`{selected_regs}`, however, for some of the targets (for example MIPS),
  clearing some registers that are in the :samp:`{selected_regs}` requires
  clearing other call used registers that are not in the :samp:`{selected_regs}`,
  under such situation, the returned set of registers must be a subset of all
  call used registers.

  The default implementation uses normal move instructions to zero
  all the registers in :samp:`{selected_regs}`.  Define this hook if the
  target has more efficient ways of zeroing certain registers,
  or if you believe that certain registers would never contain
  values that are useful to an attacker.

.. hook-end

.. function:: bool TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS (void)

  .. hook-start:TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS

  When optimization is disabled, this hook indicates whether or not
  arguments should be allocated to stack slots.  Normally, GCC allocates
  stacks slots for arguments when not optimizing in order to make
  debugging easier.  However, when a function is declared with
  ``__attribute__((naked))``, there is no stack frame, and the compiler
  cannot safely move arguments from the registers in which they are passed
  to the stack.  Therefore, this hook should return true in general, but
  false for naked functions.  The default implementation always returns true.

.. hook-end

.. c:var:: unsigned HOST_WIDE_INT TARGET_CONST_ANCHOR

  .. hook-start:TARGET_CONST_ANCHOR

  On some architectures it can take multiple instructions to synthesize
  a constant.  If there is another constant already in a register that
  is close enough in value then it is preferable that the new constant
  is computed from this register using immediate addition or
  subtraction.  We accomplish this through CSE.  Besides the value of
  the constant we also add a lower and an upper constant anchor to the
  available expressions.  These are then queried when encountering new
  constants.  The anchors are computed by rounding the constant up and
  down to a multiple of the value of ``TARGET_CONST_ANCHOR``.
  ``TARGET_CONST_ANCHOR`` should be the maximum positive value
  accepted by immediate-add plus one.  We currently assume that the
  value of ``TARGET_CONST_ANCHOR`` is a power of 2.  For example, on
  MIPS, where add-immediate takes a 16-bit signed value,
  ``TARGET_CONST_ANCHOR`` is set to :samp:`0x8000`.  The default value
  is zero, which disables this optimization.

.. hook-end

.. function:: unsigned HOST_WIDE_INT TARGET_ASAN_SHADOW_OFFSET (void)

  .. hook-start:TARGET_ASAN_SHADOW_OFFSET

  Return the offset bitwise ored into shifted address to get corresponding
  Address Sanitizer shadow memory address.  NULL if Address Sanitizer is not
  supported by the target.  May return 0 if Address Sanitizer is not supported
  by a subtarget.

.. hook-end

.. function:: unsigned HOST_WIDE_INT TARGET_MEMMODEL_CHECK (unsigned HOST_WIDE_INT val)

  .. hook-start:TARGET_MEMMODEL_CHECK

  Validate target specific memory model mask bits. When NULL no target specific
  memory model bits are allowed.

.. hook-end

.. c:var:: unsigned char TARGET_ATOMIC_TEST_AND_SET_TRUEVAL

  .. hook-start:TARGET_ATOMIC_TEST_AND_SET_TRUEVAL

  This value should be set if the result written by
  ``atomic_test_and_set`` is not exactly 1, i.e. the
  ``bool`` ``true``.

.. hook-end

.. function:: bool TARGET_HAS_IFUNC_P (void)

  .. hook-start:TARGET_HAS_IFUNC_P

  It returns true if the target supports GNU indirect functions.
  The support includes the assembler, linker and dynamic linker.
  The default value of this hook is based on target's libc.

.. hook-end

.. function:: bool TARGET_IFUNC_REF_LOCAL_OK (void)

   .. hook-start:TARGET_IFUNC_REF_LOCAL_OK

  Return true if it is OK to reference indirect function resolvers
  locally.  The default is to return false.

.. hook-end

.. function:: unsigned int TARGET_ATOMIC_ALIGN_FOR_MODE (machine_mode mode)

  .. hook-start:TARGET_ATOMIC_ALIGN_FOR_MODE

  If defined, this function returns an appropriate alignment in bits for an
  atomic object of machine_mode :samp:`{mode}`.  If 0 is returned then the
  default alignment for the specified mode is used.

.. hook-end

.. function:: void TARGET_ATOMIC_ASSIGN_EXPAND_FENV (tree *hold, tree *clear, tree *update)

  .. hook-start:TARGET_ATOMIC_ASSIGN_EXPAND_FENV

  ISO C11 requires atomic compound assignments that may raise floating-point
  exceptions to raise exceptions corresponding to the arithmetic operation
  whose result was successfully stored in a compare-and-exchange sequence.
  This requires code equivalent to calls to ``feholdexcept``,
  ``feclearexcept`` and ``feupdateenv`` to be generated at
  appropriate points in the compare-and-exchange sequence.  This hook should
  set ``*hold`` to an expression equivalent to the call to
  ``feholdexcept``, ``*clear`` to an expression equivalent to
  the call to ``feclearexcept`` and ``*update`` to an expression
  equivalent to the call to ``feupdateenv``.  The three expressions are
  ``NULL_TREE`` on entry to the hook and may be left as ``NULL_TREE``
  if no code is required in a particular place.  The default implementation
  leaves all three expressions as ``NULL_TREE``.  The
  ``__atomic_feraiseexcept`` function from ``libatomic`` may be of use
  as part of the code generated in ``*update``.

.. hook-end

.. function:: void TARGET_RECORD_OFFLOAD_SYMBOL (tree)

  .. hook-start:TARGET_RECORD_OFFLOAD_SYMBOL

  Used when offloaded functions are seen in the compilation unit and no named
  sections are available.  It is called once for each symbol that must be
  recorded in the offload function and variable table.

.. hook-end

.. function:: char * TARGET_OFFLOAD_OPTIONS (void)

  .. hook-start:TARGET_OFFLOAD_OPTIONS

  Used when writing out the list of options into an LTO file.  It should
  translate any relevant target-specific options (such as the ABI in use)
  into one of the :option:`-foffload` options that exist as a common interface
  to express such options.  It should return a string containing these options,
  separated by spaces, which the caller will free.

.. hook-end

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

.. function:: bool TARGET_HAVE_SPECULATION_SAFE_VALUE (bool active)

  .. hook-start:TARGET_HAVE_SPECULATION_SAFE_VALUE

  This hook is used to determine the level of target support for
  ``__builtin_speculation_safe_value``.  If called with an argument
  of false, it returns true if the target has been modified to support
  this builtin.  If called with an argument of true, it returns true
  if the target requires active mitigation execution might be speculative.

  The default implementation returns false if the target does not define
  a pattern named ``speculation_barrier``.  Else it returns true
  for the first case and whether the pattern is enabled for the current
  compilation for the second case.

  For targets that have no processors that can execute instructions
  speculatively an alternative implemenation of this hook is available:
  simply redefine this hook to ``speculation_safe_value_not_needed``
  along with your other target hooks.

.. hook-end

.. function:: rtx TARGET_SPECULATION_SAFE_VALUE (machine_mode mode, rtx result, rtx val, rtx failval)

  .. hook-start:TARGET_SPECULATION_SAFE_VALUE

  This target hook can be used to generate a target-specific code
  sequence that implements the ``__builtin_speculation_safe_value``
  built-in function.  The function must always return :samp:`{val}` in
  :samp:`{result}` in mode :samp:`{mode}` when the cpu is not executing
  speculatively, but must never return that when speculating until it
  is known that the speculation will not be unwound.  The hook supports
  two primary mechanisms for implementing the requirements.  The first
  is to emit a speculation barrier which forces the processor to wait
  until all prior speculative operations have been resolved; the second
  is to use a target-specific mechanism that can track the speculation
  state and to return :samp:`{failval}` if it can determine that
  speculation must be unwound at a later time.

  The default implementation simply copies :samp:`{val}` to :samp:`{result}` and
  emits a ``speculation_barrier`` instruction if that is defined.

.. hook-end

.. function:: void TARGET_RUN_TARGET_SELFTESTS (void)

  .. hook-start:TARGET_RUN_TARGET_SELFTESTS

  If selftests are enabled, run any selftests for this target.

.. hook-end

.. function:: bool TARGET_MEMTAG_CAN_TAG_ADDRESSES ()

  .. hook-start:TARGET_MEMTAG_CAN_TAG_ADDRESSES

  True if the backend architecture naturally supports ignoring some region
  of pointers.  This feature means that :option:`-fsanitize=hwaddress` can
  work.

  At preset, this feature does not support address spaces.  It also requires
  ``Pmode`` to be the same as ``ptr_mode``.

.. hook-end

.. function:: uint8_t TARGET_MEMTAG_TAG_SIZE ()

  .. hook-start:TARGET_MEMTAG_TAG_SIZE

  Return the size of a tag (in bits) for this platform.

  The default returns 8.

.. hook-end

.. function:: uint8_t TARGET_MEMTAG_GRANULE_SIZE ()

  .. hook-start:TARGET_MEMTAG_GRANULE_SIZE

  Return the size in real memory that each byte in shadow memory refers to.
  I.e. if a variable is :samp:`{X}` bytes long in memory, then this hook should
  return the value :samp:`{Y}` such that the tag in shadow memory spans
  :samp:`{X}` / :samp:`{Y}` bytes.

  Most variables will need to be aligned to this amount since two variables
  that are neighbors in memory and share a tag granule would need to share
  the same tag.

  The default returns 16.

.. hook-end

.. function:: rtx TARGET_MEMTAG_INSERT_RANDOM_TAG (rtx untagged, rtx target)

  .. hook-start:TARGET_MEMTAG_INSERT_RANDOM_TAG

  Return an RTX representing the value of :samp:`{untagged}` but with a
  (possibly) random tag in it.
  Put that value into :samp:`{target}` if it is convenient to do so.
  This function is used to generate a tagged base for the current stack frame.

.. hook-end

.. function:: rtx TARGET_MEMTAG_ADD_TAG (rtx base, poly_int64 addr_offset, uint8_t tag_offset)

  .. hook-start:TARGET_MEMTAG_ADD_TAG

  Return an RTX that represents the result of adding :samp:`{addr_offset}` to
  the address in pointer :samp:`{base}` and :samp:`{tag_offset}` to the tag in pointer
  :samp:`{base}`.
  The resulting RTX must either be a valid memory address or be able to get
  put into an operand with ``force_operand``.

  Unlike other memtag hooks, this must return an expression and not emit any
  RTL.

.. hook-end

.. function:: rtx TARGET_MEMTAG_SET_TAG (rtx untagged_base, rtx tag, rtx target)

  .. hook-start:TARGET_MEMTAG_SET_TAG

  Return an RTX representing :samp:`{untagged_base}` but with the tag :samp:`{tag}`.
  Try and store this in :samp:`{target}` if convenient.
  :samp:`{untagged_base}` is required to have a zero tag when this hook is called.
  The default of this hook is to set the top byte of :samp:`{untagged_base}` to
  :samp:`{tag}`.

.. hook-end

.. function:: rtx TARGET_MEMTAG_EXTRACT_TAG (rtx tagged_pointer, rtx target)

  .. hook-start:TARGET_MEMTAG_EXTRACT_TAG

  Return an RTX representing the tag stored in :samp:`{tagged_pointer}`.
  Store the result in :samp:`{target}` if it is convenient.
  The default represents the top byte of the original pointer.

.. hook-end

.. function:: rtx TARGET_MEMTAG_UNTAGGED_POINTER (rtx tagged_pointer, rtx target)

  .. hook-start:TARGET_MEMTAG_UNTAGGED_POINTER

  Return an RTX representing :samp:`{tagged_pointer}` with its tag set to zero.
  Store the result in :samp:`{target}` if convenient.
  The default clears the top byte of the original pointer.

.. hook-end

.. function:: HOST_WIDE_INT TARGET_GCOV_TYPE_SIZE (void)

  .. hook-start:TARGET_GCOV_TYPE_SIZE

  Returns the gcov type size in bits.  This type is used for example for
  counters incremented by profiling and code-coverage events.  The default
  value is 64, if the type size of long long is greater than 32, otherwise the
  default value is 32.  A 64-bit type is recommended to avoid overflows of the
  counters.  If the :option:`-fprofile-update=atomic` is used, then the
  counters are incremented using atomic operations.  Targets not supporting
  64-bit atomic operations may override the default value and request a 32-bit
  type.

.. hook-end

.. c:var:: bool TARGET_HAVE_SHADOW_CALL_STACK

  .. hook-start:TARGET_HAVE_SHADOW_CALL_STACK

  This value is true if the target platform supports
  :option:`-fsanitize=shadow-call-stack`.  The default value is false.

.. hook-end