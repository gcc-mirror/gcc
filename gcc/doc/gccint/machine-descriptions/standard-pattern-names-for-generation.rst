..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: standard pattern names, pattern names, names, pattern

.. _standard-names:

Standard Pattern Names For Generation
*************************************

Here is a table of the instruction names that are meaningful in the RTL
generation pass of the compiler.  Giving one of these names to an
instruction pattern tells the RTL generation pass that it can use the
pattern to accomplish a certain task.

.. index:: movm instruction pattern

movm
  Here :samp:`{m}` stands for a two-letter machine mode name, in lowercase.
  This instruction pattern moves data with that machine mode from operand
  1 to operand 0.  For example, :samp:`movsi` moves full-word data.

  If operand 0 is a ``subreg`` with mode :samp:`{m}` of a register whose
  own mode is wider than :samp:`{m}`, the effect of this instruction is
  to store the specified value in the part of the register that corresponds
  to mode :samp:`{m}`.  Bits outside of :samp:`{m}`, but which are within the
  same target word as the ``subreg`` are undefined.  Bits which are
  outside the target word are left unchanged.

  This class of patterns is special in several ways.  First of all, each
  of these names up to and including full word size *must* be defined,
  because there is no other way to copy a datum from one place to another.
  If there are patterns accepting operands in larger modes,
  :samp:`mov{m}` must be defined for integer modes of those sizes.

  Second, these patterns are not used solely in the RTL generation pass.
  Even the reload pass can generate move insns to copy values from stack
  slots into temporary registers.  When it does so, one of the operands is
  a hard register and the other is an operand that can need to be reloaded
  into a register.

  .. index:: force_reg

  Therefore, when given such a pair of operands, the pattern must generate
  RTL which needs no reloading and needs no temporary registers---no
  registers other than the operands.  For example, if you support the
  pattern with a ``define_expand``, then in such a case the
  ``define_expand`` mustn't call ``force_reg`` or any other such
  function which might generate new pseudo registers.

  This requirement exists even for subword modes on a RISC machine where
  fetching those modes from memory normally requires several insns and
  some temporary registers.

  .. index:: change_address

  During reload a memory reference with an invalid address may be passed
  as an operand.  Such an address will be replaced with a valid address
  later in the reload pass.  In this case, nothing may be done with the
  address except to use it as it stands.  If it is copied, it will not be
  replaced with a valid address.  No attempt should be made to make such
  an address into a valid address and no routine (such as
  ``change_address``) that will do so may be called.  Note that
  ``general_operand`` will fail when applied to such an address.

  .. index:: reload_in_progress

  The global variable ``reload_in_progress`` (which must be explicitly
  declared if required) can be used to determine whether such special
  handling is required.

  The variety of operands that have reloads depends on the rest of the
  machine description, but typically on a RISC machine these can only be
  pseudo registers that did not get hard registers, while on other
  machines explicit memory references will get optional reloads.

  If a scratch register is required to move an object to or from memory,
  it can be allocated using ``gen_reg_rtx`` prior to life analysis.

  If there are cases which need scratch registers during or after reload,
  you must provide an appropriate secondary_reload target hook.

  .. index:: can_create_pseudo_p

  The macro ``can_create_pseudo_p`` can be used to determine if it
  is unsafe to create new pseudo registers.  If this variable is nonzero, then
  it is unsafe to call ``gen_reg_rtx`` to allocate a new pseudo.

  The constraints on a :samp:`mov{m}` must permit moving any hard
  register to any other hard register provided that
  ``TARGET_HARD_REGNO_MODE_OK`` permits mode :samp:`{m}` in both registers and
  ``TARGET_REGISTER_MOVE_COST`` applied to their classes returns a value
  of 2.

  It is obligatory to support floating point :samp:`mov{m}`
  instructions into and out of any registers that can hold fixed point
  values, because unions and structures (which have modes ``SImode`` or
  ``DImode``) can be in those registers and they may have floating
  point members.

  There may also be a need to support fixed point :samp:`mov{m}`
  instructions in and out of floating point registers.  Unfortunately, I
  have forgotten why this was so, and I don't know whether it is still
  true.  If ``TARGET_HARD_REGNO_MODE_OK`` rejects fixed point values in
  floating point registers, then the constraints of the fixed point
  :samp:`mov{m}` instructions must be designed to avoid ever trying to
  reload into a floating point register.

  .. index:: reload_in instruction pattern, reload_out instruction pattern

reload_inm reload_outm
  These named patterns have been obsoleted by the target hook
  ``secondary_reload``.

  Like :samp:`mov{m}`, but used when a scratch register is required to
  move between operand 0 and operand 1.  Operand 2 describes the scratch
  register.  See the discussion of the ``SECONDARY_RELOAD_CLASS``
  macro in see :ref:`register-classes`.

  There are special restrictions on the form of the ``match_operand`` s
  used in these patterns.  First, only the predicate for the reload
  operand is examined, i.e., ``reload_in`` examines operand 1, but not
  the predicates for operand 0 or 2.  Second, there may be only one
  alternative in the constraints.  Third, only a single register class
  letter may be used for the constraint; subsequent constraint letters
  are ignored.  As a special exception, an empty constraint string
  matches the ``ALL_REGS`` register class.  This may relieve ports
  of the burden of defining an ``ALL_REGS`` constraint letter just
  for these patterns.

  .. index:: movstrictm instruction pattern

movstrictm
  Like :samp:`mov{m}` except that if operand 0 is a ``subreg``
  with mode :samp:`{m}` of a register whose natural mode is wider,
  the :samp:`movstrict{m}` instruction is guaranteed not to alter
  any of the register except the part which belongs to mode :samp:`{m}`.

  .. index:: movmisalignm instruction pattern

movmisalignm
  This variant of a move pattern is designed to load or store a value
  from a memory address that is not naturally aligned for its mode.
  For a store, the memory will be in operand 0; for a load, the memory
  will be in operand 1.  The other operand is guaranteed not to be a
  memory, so that it's easy to tell whether this is a load or store.

  This pattern is used by the autovectorizer, and when expanding a
  ``MISALIGNED_INDIRECT_REF`` expression.

  .. index:: load_multiple instruction pattern

load_multiple
  Load several consecutive memory locations into consecutive registers.
  Operand 0 is the first of the consecutive registers, operand 1
  is the first memory location, and operand 2 is a constant: the
  number of consecutive registers.

  Define this only if the target machine really has such an instruction;
  do not define this if the most efficient way of loading consecutive
  registers from memory is to do them one at a time.

  On some machines, there are restrictions as to which consecutive
  registers can be stored into memory, such as particular starting or
  ending register numbers or only a range of valid counts.  For those
  machines, use a ``define_expand`` (see :ref:`expander-definitions`)
  and make the pattern fail if the restrictions are not met.

  Write the generated insn as a ``parallel`` with elements being a
  ``set`` of one register from the appropriate memory location (you may
  also need ``use`` or ``clobber`` elements).  Use a
  ``match_parallel`` (see :ref:`rtl-template`) to recognize the insn.  See
  :samp:`rs6000.md` for examples of the use of this insn pattern.

  .. index:: store_multiple instruction pattern

store_multiple
  Similar to :samp:`load_multiple`, but store several consecutive registers
  into consecutive memory locations.  Operand 0 is the first of the
  consecutive memory locations, operand 1 is the first register, and
  operand 2 is a constant: the number of consecutive registers.

  .. index:: vec_load_lanesmn instruction pattern

vec_load_lanesmn
  Perform an interleaved load of several vectors from memory operand 1
  into register operand 0.  Both operands have mode :samp:`{m}`.  The register
  operand is viewed as holding consecutive vectors of mode :samp:`{n}`,
  while the memory operand is a flat array that contains the same number
  of elements.  The operation is equivalent to:

  .. code-block:: c++

    int c = GET_MODE_SIZE (m) / GET_MODE_SIZE (n);
    for (j = 0; j < GET_MODE_NUNITS (n); j++)
      for (i = 0; i < c; i++)
        operand0[i][j] = operand1[j * c + i];

  For example, :samp:`vec_load_lanestiv4hi` loads 8 16-bit values
  from memory into a register of mode :samp:`TI`.  The register
  contains two consecutive vectors of mode :samp:`V4HI`.

  This pattern can only be used if:

  .. code-block:: c++

    TARGET_ARRAY_MODE_SUPPORTED_P (n, c)

  is true.  GCC assumes that, if a target supports this kind of
  instruction for some mode :samp:`{n}`, it also supports unaligned
  loads for vectors of mode :samp:`{n}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: vec_mask_load_lanesmn instruction pattern

vec_mask_load_lanesmn
  Like :samp:`vec_load_lanes{m}{n}`, but takes an additional
  mask operand (operand 2) that specifies which elements of the destination
  vectors should be loaded.  Other elements of the destination
  vectors are set to zero.  The operation is equivalent to:

  .. code-block:: c++

    int c = GET_MODE_SIZE (m) / GET_MODE_SIZE (n);
    for (j = 0; j < GET_MODE_NUNITS (n); j++)
      if (operand2[j])
        for (i = 0; i < c; i++)
          operand0[i][j] = operand1[j * c + i];
      else
        for (i = 0; i < c; i++)
          operand0[i][j] = 0;

  This pattern is not allowed to ``FAIL``.

  .. index:: vec_store_lanesmn instruction pattern

vec_store_lanesmn
  Equivalent to :samp:`vec_load_lanes{m}{n}`, with the memory
  and register operands reversed.  That is, the instruction is
  equivalent to:

  .. code-block:: c++

    int c = GET_MODE_SIZE (m) / GET_MODE_SIZE (n);
    for (j = 0; j < GET_MODE_NUNITS (n); j++)
      for (i = 0; i < c; i++)
        operand0[j * c + i] = operand1[i][j];

  for a memory operand 0 and register operand 1.

  This pattern is not allowed to ``FAIL``.

  .. index:: vec_mask_store_lanesmn instruction pattern

vec_mask_store_lanesmn
  Like :samp:`vec_store_lanes{m}{n}`, but takes an additional
  mask operand (operand 2) that specifies which elements of the source
  vectors should be stored.  The operation is equivalent to:

  .. code-block:: c++

    int c = GET_MODE_SIZE (m) / GET_MODE_SIZE (n);
    for (j = 0; j < GET_MODE_NUNITS (n); j++)
      if (operand2[j])
        for (i = 0; i < c; i++)
          operand0[j * c + i] = operand1[i][j];

  This pattern is not allowed to ``FAIL``.

  .. index:: gather_loadmn instruction pattern

gather_loadmn
  Load several separate memory locations into a vector of mode :samp:`{m}`.
  Operand 1 is a scalar base address and operand 2 is a vector of mode :samp:`{n}`
  containing offsets from that base.  Operand 0 is a destination vector with
  the same number of elements as :samp:`{n}`.  For each element index :samp:`{i}` :

  * extend the offset element :samp:`{i}` to address width, using zero
    extension if operand 3 is 1 and sign extension if operand 3 is zero;

  * multiply the extended offset by operand 4;

  * add the result to the base; and

  * load the value at that address into element :samp:`{i}` of operand 0.

  The value of operand 3 does not matter if the offsets are already
  address width.

  .. index:: mask_gather_loadmn instruction pattern

mask_gather_loadmn
  Like :samp:`gather_load{m}{n}`, but takes an extra mask operand as
  operand 5.  Bit :samp:`{i}` of the mask is set if element :samp:`{i}`
  of the result should be loaded from memory and clear if element :samp:`{i}`
  of the result should be set to zero.

  .. index:: scatter_storemn instruction pattern

scatter_storemn
  Store a vector of mode :samp:`{m}` into several distinct memory locations.
  Operand 0 is a scalar base address and operand 1 is a vector of mode
  :samp:`{n}` containing offsets from that base.  Operand 4 is the vector of
  values that should be stored, which has the same number of elements as
  :samp:`{n}`.  For each element index :samp:`{i}` :

  * extend the offset element :samp:`{i}` to address width, using zero
    extension if operand 2 is 1 and sign extension if operand 2 is zero;

  * multiply the extended offset by operand 3;

  * add the result to the base; and

  * store element :samp:`{i}` of operand 4 to that address.

  The value of operand 2 does not matter if the offsets are already
  address width.

  .. index:: mask_scatter_storemn instruction pattern

mask_scatter_storemn
  Like :samp:`scatter_store{m}{n}`, but takes an extra mask operand as
  operand 5.  Bit :samp:`{i}` of the mask is set if element :samp:`{i}`
  of the result should be stored to memory.

  .. index:: vec_setm instruction pattern

vec_setm
  Set given field in the vector value.  Operand 0 is the vector to modify,
  operand 1 is new value of field and operand 2 specify the field index.

  .. index:: vec_extractmn instruction pattern

vec_extractmn
  Extract given field from the vector value.  Operand 1 is the vector, operand 2
  specify field index and operand 0 place to store value into.  The
  :samp:`{n}` mode is the mode of the field or vector of fields that should be
  extracted, should be either element mode of the vector mode :samp:`{m}`, or
  a vector mode with the same element mode and smaller number of elements.
  If :samp:`{n}` is a vector mode, the index is counted in units of that mode.

  .. index:: vec_initmn instruction pattern

vec_initmn
  Initialize the vector to given values.  Operand 0 is the vector to initialize
  and operand 1 is parallel containing values for individual fields.  The
  :samp:`{n}` mode is the mode of the elements, should be either element mode of
  the vector mode :samp:`{m}`, or a vector mode with the same element mode and
  smaller number of elements.

  .. index:: vec_duplicatem instruction pattern

vec_duplicatem
  Initialize vector output operand 0 so that each element has the value given
  by scalar input operand 1.  The vector has mode :samp:`{m}` and the scalar has
  the mode appropriate for one element of :samp:`{m}`.

  This pattern only handles duplicates of non-constant inputs.  Constant
  vectors go through the ``movm`` pattern instead.

  This pattern is not allowed to ``FAIL``.

  .. index:: vec_seriesm instruction pattern

vec_seriesm
  Initialize vector output operand 0 so that element :samp:`{i}` is equal to
  operand 1 plus :samp:`{i}` times operand 2.  In other words, create a linear
  series whose base value is operand 1 and whose step is operand 2.

  The vector output has mode :samp:`{m}` and the scalar inputs have the mode
  appropriate for one element of :samp:`{m}`.  This pattern is not used for
  floating-point vectors, in order to avoid having to specify the
  rounding behavior for :samp:`{i}` > 1.

  This pattern is not allowed to ``FAIL``.

  .. index:: while_ultmn instruction pattern

while_ultmn
  Set operand 0 to a mask that is true while incrementing operand 1
  gives a value that is less than operand 2, for a vector length up to operand 3.
  Operand 0 has mode :samp:`{n}` and operands 1 and 2 are scalar integers of mode
  :samp:`{m}`.  Operand 3 should be omitted when :samp:`{n}` is a vector mode, and
  a ``CONST_INT`` otherwise.  The operation for vector modes is equivalent to:

  .. code-block:: c++

    operand0[0] = operand1 < operand2;
    for (i = 1; i < GET_MODE_NUNITS (n); i++)
      operand0[i] = operand0[i - 1] && (operand1 + i < operand2);

  And for non-vector modes the operation is equivalent to:

  .. code-block:: c++

    operand0[0] = operand1 < operand2;
    for (i = 1; i < operand3; i++)
      operand0[i] = operand0[i - 1] && (operand1 + i < operand2);

  .. index:: check_raw_ptrsm instruction pattern

check_raw_ptrsm
  Check whether, given two pointers :samp:`{a}` and :samp:`{b}` and a length :samp:`{len}`,
  a write of :samp:`{len}` bytes at :samp:`{a}` followed by a read of :samp:`{len}` bytes
  at :samp:`{b}` can be split into interleaved byte accesses
  :samp:`{a}[0], {b}[0], {a}[1], {b}[1], ...`
  without affecting the dependencies between the bytes.  Set operand 0
  to true if the split is possible and false otherwise.

  Operands 1, 2 and 3 provide the values of :samp:`{a}`, :samp:`{b}` and :samp:`{len}`
  respectively.  Operand 4 is a constant integer that provides the known
  common alignment of :samp:`{a}` and :samp:`{b}`.  All inputs have mode :samp:`{m}`.

  This split is possible if:

  .. code-block:: c++

    a == b || a + len <= b || b + len <= a

  You should only define this pattern if the target has a way of accelerating
  the test without having to do the individual comparisons.

  .. index:: check_war_ptrsm instruction pattern

check_war_ptrsm
  Like :samp:`check_raw_ptrs{m}`, but with the read and write swapped round.
  The split is possible in this case if:

  .. code-block:: c++

    b <= a || a + len <= b

  .. index:: vec_cmpmn instruction pattern

vec_cmpmn
  Output a vector comparison.  Operand 0 of mode :samp:`{n}` is the destination for
  predicate in operand 1 which is a signed vector comparison with operands of
  mode :samp:`{m}` in operands 2 and 3.  Predicate is computed by element-wise
  evaluation of the vector comparison with a truth value of all-ones and a false
  value of all-zeros.

  .. index:: vec_cmpumn instruction pattern

vec_cmpumn
  Similar to ``vec_cmpmn`` but perform unsigned vector comparison.

  .. index:: vec_cmpeqmn instruction pattern

vec_cmpeqmn
  Similar to ``vec_cmpmn`` but perform equality or non-equality
  vector comparison only.  If ``vec_cmpmn``
  or ``vec_cmpumn`` instruction pattern is supported,
  it will be preferred over ``vec_cmpeqmn``, so there is
  no need to define this instruction pattern if the others are supported.

  .. index:: vcondmn instruction pattern

vcondmn
  Output a conditional vector move.  Operand 0 is the destination to
  receive a combination of operand 1 and operand 2, which are of mode :samp:`{m}`,
  dependent on the outcome of the predicate in operand 3 which is a signed
  vector comparison with operands of mode :samp:`{n}` in operands 4 and 5.  The
  modes :samp:`{m}` and :samp:`{n}` should have the same size.  Operand 0
  will be set to the value :samp:`{op1}` & :samp:`{msk}` | :samp:`{op2}` & ~ :samp:`{msk}`
  where :samp:`{msk}` is computed by element-wise evaluation of the vector
  comparison with a truth value of all-ones and a false value of all-zeros.

  .. index:: vcondumn instruction pattern

vcondumn
  Similar to ``vcondmn`` but performs unsigned vector
  comparison.

  .. index:: vcondeqmn instruction pattern

vcondeqmn
  Similar to ``vcondmn`` but performs equality or
  non-equality vector comparison only.  If ``vcondmn``
  or ``vcondumn`` instruction pattern is supported,
  it will be preferred over ``vcondeqmn``, so there is
  no need to define this instruction pattern if the others are supported.

  .. index:: vcond_mask_mn instruction pattern

vcond_mask_mn
  Similar to ``vcondmn`` but operand 3 holds a pre-computed
  result of vector comparison.

  .. index:: maskloadmn instruction pattern

maskloadmn
  Perform a masked load of vector from memory operand 1 of mode :samp:`{m}`
  into register operand 0.  Mask is provided in register operand 2 of
  mode :samp:`{n}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: maskstoremn instruction pattern

maskstoremn
  Perform a masked store of vector from register operand 1 of mode :samp:`{m}`
  into memory operand 0.  Mask is provided in register operand 2 of
  mode :samp:`{n}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: len_load_m instruction pattern

len_load_m
  Load (operand 2 - operand 3) elements from vector memory operand 1
  into vector register operand 0, setting the other elements of
  operand 0 to undefined values.  Operands 0 and 1 have mode :samp:`{m}`,
  which must be a vector mode.  Operand 2 has whichever integer mode the
  target prefers.  Operand 3 conceptually has mode ``QI``.

  Operand 2 can be a variable or a constant amount.  Operand 3 specifies a
  constant bias: it is either a constant 0 or a constant -1.  The predicate on
  operand 3 must only accept the bias values that the target actually supports.
  GCC handles a bias of 0 more efficiently than a bias of -1.

  If (operand 2 - operand 3) exceeds the number of elements in mode
  :samp:`{m}`, the behavior is undefined.

  If the target prefers the length to be measured in bytes rather than
  elements, it should only implement this pattern for vectors of ``QI``
  elements.

  This pattern is not allowed to ``FAIL``.

  .. index:: len_store_m instruction pattern

len_store_m
  Store (operand 2 - operand 3) vector elements from vector register operand 1
  into memory operand 0, leaving the other elements of
  operand 0 unchanged.  Operands 0 and 1 have mode :samp:`{m}`, which must be
  a vector mode.  Operand 2 has whichever integer mode the target prefers.
  Operand 3 conceptually has mode ``QI``.

  Operand 2 can be a variable or a constant amount.  Operand 3 specifies a
  constant bias: it is either a constant 0 or a constant -1.  The predicate on
  operand 3 must only accept the bias values that the target actually supports.
  GCC handles a bias of 0 more efficiently than a bias of -1.

  If (operand 2 - operand 3) exceeds the number of elements in mode
  :samp:`{m}`, the behavior is undefined.

  If the target prefers the length to be measured in bytes
  rather than elements, it should only implement this pattern for vectors
  of ``QI`` elements.

  This pattern is not allowed to ``FAIL``.

  .. index:: vec_permm instruction pattern

vec_permm
  Output a (variable) vector permutation.  Operand 0 is the destination
  to receive elements from operand 1 and operand 2, which are of mode
  :samp:`{m}`.  Operand 3 is the :dfn:`selector`.  It is an integral mode
  vector of the same width and number of elements as mode :samp:`{m}`.

  The input elements are numbered from 0 in operand 1 through
  2\* :samp:`{N}` -1 in operand 2.  The elements of the selector must
  be computed modulo 2\* :samp:`{N}`.  Note that if
  ``rtx_equal_p(operand1, operand2)``, this can be implemented
  with just operand 1 and selector elements modulo :samp:`{N}`.

  In order to make things easy for a number of targets, if there is no
  :samp:`vec_perm` pattern for mode :samp:`{m}`, but there is for mode :samp:`{q}`
  where :samp:`{q}` is a vector of ``QImode`` of the same width as :samp:`{m}`,
  the middle-end will lower the mode :samp:`{m}` ``VEC_PERM_EXPR`` to
  mode :samp:`{q}`.

  See also ``TARGET_VECTORIZER_VEC_PERM_CONST``, which performs
  the analogous operation for constant selectors.

  .. index:: pushm1 instruction pattern

pushm1
  Output a push instruction.  Operand 0 is value to push.  Used only when
  ``PUSH_ROUNDING`` is defined.  For historical reason, this pattern may be
  missing and in such case an ``mov`` expander is used instead, with a
  ``MEM`` expression forming the push operation.  The ``mov`` expander
  method is deprecated.

  .. index:: addm3 instruction pattern

addm3
  Add operand 2 and operand 1, storing the result in operand 0.  All operands
  must have mode :samp:`{m}`.  This can be used even on two-address machines, by
  means of constraints requiring operands 1 and 0 to be the same location.

  .. index:: ssaddm3 instruction pattern, usaddm3 instruction pattern, subm3 instruction pattern, sssubm3 instruction pattern, ussubm3 instruction pattern, mulm3 instruction pattern, ssmulm3 instruction pattern, usmulm3 instruction pattern, divm3 instruction pattern, ssdivm3 instruction pattern, udivm3 instruction pattern, usdivm3 instruction pattern, modm3 instruction pattern, umodm3 instruction pattern, uminm3 instruction pattern, umaxm3 instruction pattern, andm3 instruction pattern, iorm3 instruction pattern, xorm3 instruction pattern

:samp:`ssadd{m}3`, :samp:`usadd{m}3` :samp:`sub{m}3`, :samp:`sssub{m}3`, :samp:`ussub{m}3` :samp:`mul{m}3`, :samp:`ssmul{m}3`, :samp:`usmul{m}3` :samp:`div{m}3`, :samp:`ssdiv{m}3` :samp:`udiv{m}3`, :samp:`usdiv{m}3` :samp:`mod{m}3`, :samp:`umod{m}3` :samp:`umin{m}3`, :samp:`umax{m}3` :samp:`and{m}3`, :samp:`ior{m}3`, :samp:`xor{m}3`
  Similar, for other arithmetic operations.

  .. index:: addvm4 instruction pattern

addvm4
  Like ``addm3`` but takes a ``code_label`` as operand 3 and
  emits code to jump to it if signed overflow occurs during the addition.
  This pattern is used to implement the built-in functions performing
  signed integer addition with overflow checking.

  .. index:: subvm4 instruction pattern, mulvm4 instruction pattern

:samp:`subv{m}4`, :samp:`mulv{m}4`
  Similar, for other signed arithmetic operations.

  .. index:: uaddvm4 instruction pattern

uaddvm4
  Like ``addvm4`` but for unsigned addition.  That is to
  say, the operation is the same as signed addition but the jump
  is taken only on unsigned overflow.

  .. index:: usubvm4 instruction pattern, umulvm4 instruction pattern

:samp:`usubv{m}4`, :samp:`umulv{m}4`
  Similar, for other unsigned arithmetic operations.

  .. index:: addptrm3 instruction pattern

addptrm3
  Like ``addm3`` but is guaranteed to only be used for address
  calculations.  The expanded code is not allowed to clobber the
  condition code.  It only needs to be defined if ``addm3``
  sets the condition code.  If adds used for address calculations and
  normal adds are not compatible it is required to expand a distinct
  pattern (e.g. using an unspec).  The pattern is used by LRA to emit
  address calculations.  ``addm3`` is used if
  ``addptrm3`` is not defined.

  .. index:: fmam4 instruction pattern

fmam4
  Multiply operand 2 and operand 1, then add operand 3, storing the
  result in operand 0 without doing an intermediate rounding step.  All
  operands must have mode :samp:`{m}`.  This pattern is used to implement
  the ``fma``, ``fmaf``, and ``fmal`` builtin functions from
  the ISO C99 standard.

  .. index:: fmsm4 instruction pattern

fmsm4
  Like ``fmam4``, except operand 3 subtracted from the
  product instead of added to the product.  This is represented
  in the rtl as

  .. code-block:: c++

    (fma:m op1 op2 (neg:m op3))

  .. index:: fnmam4 instruction pattern

fnmam4
  Like ``fmam4`` except that the intermediate product
  is negated before being added to operand 3.  This is represented
  in the rtl as

  .. code-block:: c++

    (fma:m (neg:m op1) op2 op3)

  .. index:: fnmsm4 instruction pattern

fnmsm4
  Like ``fmsm4`` except that the intermediate product
  is negated before subtracting operand 3.  This is represented
  in the rtl as

  .. code-block:: c++

    (fma:m (neg:m op1) op2 (neg:m op3))

  .. index:: minm3 instruction pattern, maxm3 instruction pattern

:samp:`smin{m}3`, :samp:`smax{m}3`
  Signed minimum and maximum operations.  When used with floating point,
  if both operands are zeros, or if either operand is ``NaN``, then
  it is unspecified which of the two operands is returned as the result.

  .. index:: fminm3 instruction pattern, fmaxm3 instruction pattern

:samp:`fmin{m}3`, :samp:`fmax{m}3`
  IEEE-conformant minimum and maximum operations.  If one operand is a quiet
  ``NaN``, then the other operand is returned.  If both operands are quiet
  ``NaN``, then a quiet ``NaN`` is returned.  In the case when gcc supports
  signaling ``NaN`` (-fsignaling-nans) an invalid floating point exception is
  raised and a quiet ``NaN`` is returned.

  All operands have mode :samp:`{m}`, which is a scalar or vector
  floating-point mode.  These patterns are not allowed to ``FAIL``.

  .. index:: reduc_smin_scal_m instruction pattern, reduc_smax_scal_m instruction pattern

:samp:`reduc_smin_scal_{m}`, :samp:`reduc_smax_scal_{m}`
  Find the signed minimum/maximum of the elements of a vector. The vector is
  operand 1, and operand 0 is the scalar result, with mode equal to the mode of
  the elements of the input vector.

  .. index:: reduc_umin_scal_m instruction pattern, reduc_umax_scal_m instruction pattern

:samp:`reduc_umin_scal_{m}`, :samp:`reduc_umax_scal_{m}`
  Find the unsigned minimum/maximum of the elements of a vector. The vector is
  operand 1, and operand 0 is the scalar result, with mode equal to the mode of
  the elements of the input vector.

  .. index:: reduc_fmin_scal_m instruction pattern, reduc_fmax_scal_m instruction pattern

:samp:`reduc_fmin_scal_{m}`, :samp:`reduc_fmax_scal_{m}`
  Find the floating-point minimum/maximum of the elements of a vector,
  using the same rules as ``fminm3`` and ``fmaxm3``.
  Operand 1 is a vector of mode :samp:`{m}` and operand 0 is the scalar
  result, which has mode ``GET_MODE_INNER (m)``.

  .. index:: reduc_plus_scal_m instruction pattern

reduc_plus_scal_m
  Compute the sum of the elements of a vector. The vector is operand 1, and
  operand 0 is the scalar result, with mode equal to the mode of the elements of
  the input vector.

  .. index:: reduc_and_scal_m instruction pattern

reduc_and_scal_m, reduc_ior_scal_m, reduc_xor_scal_m
  Compute the bitwise ``AND`` / ``IOR`` / ``XOR`` reduction of the elements
  of a vector of mode :samp:`{m}`.  Operand 1 is the vector input and operand 0
  is the scalar result.  The mode of the scalar result is the same as one
  element of :samp:`{m}`.

  .. index:: extract_last_m instruction pattern

extract_last_m
  Find the last set bit in mask operand 1 and extract the associated element
  of vector operand 2.  Store the result in scalar operand 0.  Operand 2
  has vector mode :samp:`{m}` while operand 0 has the mode appropriate for one
  element of :samp:`{m}`.  Operand 1 has the usual mask mode for vectors of mode
  :samp:`{m}` ; see ``TARGET_VECTORIZE_GET_MASK_MODE``.

  .. index:: fold_extract_last_m instruction pattern

fold_extract_last_m
  If any bits of mask operand 2 are set, find the last set bit, extract
  the associated element from vector operand 3, and store the result
  in operand 0.  Store operand 1 in operand 0 otherwise.  Operand 3
  has mode :samp:`{m}` and operands 0 and 1 have the mode appropriate for
  one element of :samp:`{m}`.  Operand 2 has the usual mask mode for vectors
  of mode :samp:`{m}` ; see ``TARGET_VECTORIZE_GET_MASK_MODE``.

  .. index:: fold_left_plus_m instruction pattern

fold_left_plus_m
  Take scalar operand 1 and successively add each element from vector
  operand 2.  Store the result in scalar operand 0.  The vector has
  mode :samp:`{m}` and the scalars have the mode appropriate for one
  element of :samp:`{m}`.  The operation is strictly in-order: there is
  no reassociation.

  .. index:: mask_fold_left_plus_m instruction pattern

mask_fold_left_plus_m
  Like :samp:`fold_left_plus_{m}`, but takes an additional mask operand
  (operand 3) that specifies which elements of the source vector should be added.

  .. index:: sdot_prodm instruction pattern

sdot_prodm
  Compute the sum of the products of two signed elements.
  Operand 1 and operand 2 are of the same mode. Their
  product, which is of a wider mode, is computed and added to operand 3.
  Operand 3 is of a mode equal or wider than the mode of the product. The
  result is placed in operand 0, which is of the same mode as operand 3.

  Semantically the expressions perform the multiplication in the following signs

  .. code-block:: c++

    sdot<signed op0, signed op1, signed op2, signed op3> ==
       op0 = sign-ext (op1) * sign-ext (op2) + op3
    ...

  .. index:: udot_prodm instruction pattern

udot_prodm
  Compute the sum of the products of two unsigned elements.
  Operand 1 and operand 2 are of the same mode. Their
  product, which is of a wider mode, is computed and added to operand 3.
  Operand 3 is of a mode equal or wider than the mode of the product. The
  result is placed in operand 0, which is of the same mode as operand 3.

  Semantically the expressions perform the multiplication in the following signs

  .. code-block:: c++

    udot<unsigned op0, unsigned op1, unsigned op2, unsigned op3> ==
       op0 = zero-ext (op1) * zero-ext (op2) + op3
    ...

  .. index:: usdot_prodm instruction pattern

usdot_prodm
  Compute the sum of the products of elements of different signs.
  Operand 1 must be unsigned and operand 2 signed. Their
  product, which is of a wider mode, is computed and added to operand 3.
  Operand 3 is of a mode equal or wider than the mode of the product. The
  result is placed in operand 0, which is of the same mode as operand 3.

  Semantically the expressions perform the multiplication in the following signs

  .. code-block:: c++

    usdot<signed op0, unsigned op1, signed op2, signed op3> ==
       op0 = ((signed-conv) zero-ext (op1)) * sign-ext (op2) + op3
    ...

  .. index:: ssadm instruction pattern
  .. index:: usadm instruction pattern

ssadm, usadm
  Compute the sum of absolute differences of two signed/unsigned elements.
  Operand 1 and operand 2 are of the same mode. Their absolute difference, which
  is of a wider mode, is computed and added to operand 3. Operand 3 is of a mode
  equal or wider than the mode of the absolute difference. The result is placed
  in operand 0, which is of the same mode as operand 3.

  .. index:: widen_ssumm3 instruction pattern

widen_ssumm3

.. index:: widen_usumm3 instruction pattern

widen_usumm3

  Operands 0 and 2 are of the same mode, which is wider than the mode of
  operand 1. Add operand 1 to operand 2 and place the widened result in
  operand 0. (This is used express accumulation of elements into an accumulator
  of a wider mode.)

.. index:: smulhsm3 instruction pattern
.. index:: umulhsm3 instruction pattern

smulhsm3, umulhsm3

  Signed/unsigned multiply high with scale. This is equivalent to the C code:

  .. code-block:: c++

    narrow op0, op1, op2;
    ...
    op0 = (narrow) (((wide) op1 * (wide) op2) >> (N / 2 - 1));

  where the sign of :samp:`narrow` determines whether this is a signed
  or unsigned operation, and :samp:`{N}` is the size of :samp:`wide` in bits.

.. index:: smulhrsm3 instruction pattern
.. index:: umulhrsm3 instruction pattern

smulhrsm3, umulhrsm3

  Signed/unsigned multiply high with round and scale. This is
  equivalent to the C code:

  .. code-block:: c++

    narrow op0, op1, op2;
    ...
    op0 = (narrow) (((((wide) op1 * (wide) op2) >> (N / 2 - 2)) + 1) >> 1);

  where the sign of :samp:`narrow` determines whether this is a signed
  or unsigned operation, and :samp:`{N}` is the size of :samp:`wide` in bits.

.. index:: sdiv_pow2m3 instruction pattern

sdiv_pow2m3
  Signed division by power-of-2 immediate. Equivalent to:

  .. code-block:: c++

    signed op0, op1;
    ...
    op0 = op1 / (1 << imm);

  .. index:: vec_shl_insert_m instruction pattern

vec_shl_insert_m
  Shift the elements in vector input operand 1 left one element (i.e.
  away from element 0) and fill the vacated element 0 with the scalar
  in operand 2.  Store the result in vector output operand 0.  Operands
  0 and 1 have mode :samp:`{m}` and operand 2 has the mode appropriate for
  one element of :samp:`{m}`.

  .. index:: vec_shl_m instruction pattern

vec_shl_m
  Whole vector left shift in bits, i.e. away from element 0.
  Operand 1 is a vector to be shifted.
  Operand 2 is an integer shift amount in bits.
  Operand 0 is where the resulting shifted vector is stored.
  The output and input vectors should have the same modes.

  .. index:: vec_shr_m instruction pattern

vec_shr_m
  Whole vector right shift in bits, i.e. towards element 0.
  Operand 1 is a vector to be shifted.
  Operand 2 is an integer shift amount in bits.
  Operand 0 is where the resulting shifted vector is stored.
  The output and input vectors should have the same modes.

  .. index:: vec_pack_trunc_m instruction pattern

vec_pack_trunc_m
  Narrow (demote) and merge the elements of two vectors. Operands 1 and 2
  are vectors of the same mode having N integral or floating point elements
  of size S.  Operand 0 is the resulting vector in which 2\*N elements of
  size S/2 are concatenated after narrowing them down using truncation.

  .. index:: vec_pack_sbool_trunc_m instruction pattern

vec_pack_sbool_trunc_m
  Narrow and merge the elements of two vectors.  Operands 1 and 2 are vectors
  of the same type having N boolean elements.  Operand 0 is the resulting
  vector in which 2\*N elements are concatenated.  The last operand (operand 3)
  is the number of elements in the output vector 2\*N as a ``CONST_INT``.
  This instruction pattern is used when all the vector input and output
  operands have the same scalar mode :samp:`{m}` and thus using
  ``vec_pack_trunc_m`` would be ambiguous.

  .. index:: vec_pack_ssat_m instruction pattern, vec_pack_usat_m instruction pattern

:samp:`vec_pack_ssat_{m}`, :samp:`vec_pack_usat_{m}`
  Narrow (demote) and merge the elements of two vectors.  Operands 1 and 2
  are vectors of the same mode having N integral elements of size S.
  Operand 0 is the resulting vector in which the elements of the two input
  vectors are concatenated after narrowing them down using signed/unsigned
  saturating arithmetic.

  .. index:: vec_pack_sfix_trunc_m instruction pattern, vec_pack_ufix_trunc_m instruction pattern

:samp:`vec_pack_sfix_trunc_{m}`, :samp:`vec_pack_ufix_trunc_{m}`
  Narrow, convert to signed/unsigned integral type and merge the elements
  of two vectors.  Operands 1 and 2 are vectors of the same mode having N
  floating point elements of size S.  Operand 0 is the resulting vector
  in which 2\*N elements of size S/2 are concatenated.

  .. index:: vec_packs_float_m instruction pattern, vec_packu_float_m instruction pattern

:samp:`vec_packs_float_{m}`, :samp:`vec_packu_float_{m}`
  Narrow, convert to floating point type and merge the elements
  of two vectors.  Operands 1 and 2 are vectors of the same mode having N
  signed/unsigned integral elements of size S.  Operand 0 is the resulting vector
  in which 2\*N elements of size S/2 are concatenated.

  .. index:: vec_unpacks_hi_m instruction pattern, vec_unpacks_lo_m instruction pattern

:samp:`vec_unpacks_hi_{m}`, :samp:`vec_unpacks_lo_{m}`
  Extract and widen (promote) the high/low part of a vector of signed
  integral or floating point elements.  The input vector (operand 1) has N
  elements of size S.  Widen (promote) the high/low elements of the vector
  using signed or floating point extension and place the resulting N/2
  values of size 2\*S in the output vector (operand 0).

  .. index:: vec_unpacku_hi_m instruction pattern, vec_unpacku_lo_m instruction pattern

:samp:`vec_unpacku_hi_{m}`, :samp:`vec_unpacku_lo_{m}`
  Extract and widen (promote) the high/low part of a vector of unsigned
  integral elements.  The input vector (operand 1) has N elements of size S.
  Widen (promote) the high/low elements of the vector using zero extension and
  place the resulting N/2 values of size 2\*S in the output vector (operand 0).

  .. index:: vec_unpacks_sbool_hi_m instruction pattern, vec_unpacks_sbool_lo_m instruction pattern

:samp:`vec_unpacks_sbool_hi_{m}`, :samp:`vec_unpacks_sbool_lo_{m}`
  Extract the high/low part of a vector of boolean elements that have scalar
  mode :samp:`{m}`.  The input vector (operand 1) has N elements, the output
  vector (operand 0) has N/2 elements.  The last operand (operand 2) is the
  number of elements of the input vector N as a ``CONST_INT``.  These
  patterns are used if both the input and output vectors have the same scalar
  mode :samp:`{m}` and thus using ``vec_unpacks_hi_m`` or
  ``vec_unpacks_lo_m`` would be ambiguous.

  .. index:: vec_unpacks_float_hi_m instruction pattern, vec_unpacks_float_lo_m instruction pattern, vec_unpacku_float_hi_m instruction pattern, vec_unpacku_float_lo_m instruction pattern

:samp:`vec_unpacks_float_hi_{m}`, :samp:`vec_unpacks_float_lo_{m}` :samp:`vec_unpacku_float_hi_{m}`, :samp:`vec_unpacku_float_lo_{m}`
  Extract, convert to floating point type and widen the high/low part of a
  vector of signed/unsigned integral elements.  The input vector (operand 1)
  has N elements of size S.  Convert the high/low elements of the vector using
  floating point conversion and place the resulting N/2 values of size 2\*S in
  the output vector (operand 0).

  .. index:: vec_unpack_sfix_trunc_hi_m instruction pattern, vec_unpack_sfix_trunc_lo_m instruction pattern, vec_unpack_ufix_trunc_hi_m instruction pattern, vec_unpack_ufix_trunc_lo_m instruction pattern

:samp:`vec_unpack_sfix_trunc_hi_{m}`, vec_unpack_sfix_trunc_lo_m vec_unpack_ufix_trunc_hi_m vec_unpack_ufix_trunc_lo_m
  Extract, convert to signed/unsigned integer type and widen the high/low part of a
  vector of floating point elements.  The input vector (operand 1)
  has N elements of size S.  Convert the high/low elements of the vector
  to integers and place the resulting N/2 values of size 2\*S in
  the output vector (operand 0).

  .. index:: vec_widen_umult_hi_m instruction pattern, vec_widen_umult_lo_m instruction pattern, vec_widen_smult_hi_m instruction pattern, vec_widen_smult_lo_m instruction pattern, vec_widen_umult_even_m instruction pattern, vec_widen_umult_odd_m instruction pattern, vec_widen_smult_even_m instruction pattern, vec_widen_smult_odd_m instruction pattern

:samp:`vec_widen_umult_hi_{m}`, :samp:`vec_widen_umult_lo_{m}` :samp:`vec_widen_smult_hi_{m}`, :samp:`vec_widen_smult_lo_{m}` :samp:`vec_widen_umult_even_{m}`, :samp:`vec_widen_umult_odd_{m}` :samp:`vec_widen_smult_even_{m}`, :samp:`vec_widen_smult_odd_{m}`
  Signed/Unsigned widening multiplication.  The two inputs (operands 1 and 2)
  are vectors with N signed/unsigned elements of size S.  Multiply the high/low
  or even/odd elements of the two vectors, and put the N/2 products of size 2\*S
  in the output vector (operand 0). A target shouldn't implement even/odd pattern
  pair if it is less efficient than lo/hi one.

  .. index:: vec_widen_ushiftl_hi_m instruction pattern, vec_widen_ushiftl_lo_m instruction pattern, vec_widen_sshiftl_hi_m instruction pattern, vec_widen_sshiftl_lo_m instruction pattern

:samp:`vec_widen_ushiftl_hi_{m}`, :samp:`vec_widen_ushiftl_lo_{m}` :samp:`vec_widen_sshiftl_hi_{m}`, :samp:`vec_widen_sshiftl_lo_{m}`
  Signed/Unsigned widening shift left.  The first input (operand 1) is a vector
  with N signed/unsigned elements of size S.  Operand 2 is a constant.  Shift
  the high/low elements of operand 1, and put the N/2 results of size 2\*S in the
  output vector (operand 0).

  .. index:: vec_widen_saddl_hi_m instruction pattern, vec_widen_saddl_lo_m instruction pattern, vec_widen_uaddl_hi_m instruction pattern, vec_widen_uaddl_lo_m instruction pattern

:samp:`vec_widen_uaddl_hi_{m}`, :samp:`vec_widen_uaddl_lo_{m}` :samp:`vec_widen_saddl_hi_{m}`, :samp:`vec_widen_saddl_lo_{m}`
  Signed/Unsigned widening add long.  Operands 1 and 2 are vectors with N
  signed/unsigned elements of size S.  Add the high/low elements of 1 and 2
  together, widen the resulting elements and put the N/2 results of size 2\*S in
  the output vector (operand 0).

  .. index:: vec_widen_ssubl_hi_m instruction pattern, vec_widen_ssubl_lo_m instruction pattern, vec_widen_usubl_hi_m instruction pattern, vec_widen_usubl_lo_m instruction pattern

:samp:`vec_widen_usubl_hi_{m}`, :samp:`vec_widen_usubl_lo_{m}` :samp:`vec_widen_ssubl_hi_{m}`, :samp:`vec_widen_ssubl_lo_{m}`
  Signed/Unsigned widening subtract long.  Operands 1 and 2 are vectors with N
  signed/unsigned elements of size S.  Subtract the high/low elements of 2 from
  1 and widen the resulting elements. Put the N/2 results of size 2\*S in the
  output vector (operand 0).

  .. index:: vec_addsubm3 instruction pattern

vec_addsubm3
  Alternating subtract, add with even lanes doing subtract and odd
  lanes doing addition.  Operands 1 and 2 and the outout operand are vectors
  with mode :samp:`{m}`.

  .. index:: vec_fmaddsubm4 instruction pattern

vec_fmaddsubm4
  Alternating multiply subtract, add with even lanes doing subtract and odd
  lanes doing addition of the third operand to the multiplication result
  of the first two operands.  Operands 1, 2 and 3 and the outout operand are vectors
  with mode :samp:`{m}`.

  .. index:: vec_fmsubaddm4 instruction pattern

vec_fmsubaddm4
  Alternating multiply add, subtract with even lanes doing addition and odd
  lanes doing subtraction of the third operand to the multiplication result
  of the first two operands.  Operands 1, 2 and 3 and the outout operand are vectors
  with mode :samp:`{m}`.

  These instructions are not allowed to ``FAIL``.

  .. index:: mulhisi3 instruction pattern

mulhisi3
  Multiply operands 1 and 2, which have mode ``HImode``, and store
  a ``SImode`` product in operand 0.

  .. index:: mulqihi3 instruction pattern, mulsidi3 instruction pattern

:samp:`{mulqihi3}, {mulsidi3}`
  Similar widening-multiplication instructions of other widths.

  .. index:: umulqihi3 instruction pattern, umulhisi3 instruction pattern, umulsidi3 instruction pattern

:samp:`{umulqihi3}, {umulhisi3}, {umulsidi3}`
  Similar widening-multiplication instructions that do unsigned
  multiplication.

  .. index:: usmulqihi3 instruction pattern, usmulhisi3 instruction pattern, usmulsidi3 instruction pattern

:samp:`{usmulqihi3}, {usmulhisi3}, {usmulsidi3}`
  Similar widening-multiplication instructions that interpret the first
  operand as unsigned and the second operand as signed, then do a signed
  multiplication.

  .. index:: smulm3_highpart instruction pattern

smulm3_highpart
  Perform a signed multiplication of operands 1 and 2, which have mode
  :samp:`{m}`, and store the most significant half of the product in operand 0.
  The least significant half of the product is discarded.  This may be
  represented in RTL using a ``smul_highpart`` RTX expression.

  .. index:: umulm3_highpart instruction pattern

umulm3_highpart
  Similar, but the multiplication is unsigned.  This may be represented
  in RTL using an ``umul_highpart`` RTX expression.

  .. index:: maddmn4 instruction pattern

maddmn4
  Multiply operands 1 and 2, sign-extend them to mode :samp:`{n}`, add
  operand 3, and store the result in operand 0.  Operands 1 and 2
  have mode :samp:`{m}` and operands 0 and 3 have mode :samp:`{n}`.
  Both modes must be integer or fixed-point modes and :samp:`{n}` must be twice
  the size of :samp:`{m}`.

  In other words, ``maddmn4`` is like
  ``mulmn3`` except that it also adds operand 3.

  These instructions are not allowed to ``FAIL``.

  .. index:: umaddmn4 instruction pattern

umaddmn4
  Like ``maddmn4``, but zero-extend the multiplication
  operands instead of sign-extending them.

  .. index:: ssmaddmn4 instruction pattern

ssmaddmn4
  Like ``maddmn4``, but all involved operations must be
  signed-saturating.

  .. index:: usmaddmn4 instruction pattern

usmaddmn4
  Like ``umaddmn4``, but all involved operations must be
  unsigned-saturating.

  .. index:: msubmn4 instruction pattern

msubmn4
  Multiply operands 1 and 2, sign-extend them to mode :samp:`{n}`, subtract the
  result from operand 3, and store the result in operand 0.  Operands 1 and 2
  have mode :samp:`{m}` and operands 0 and 3 have mode :samp:`{n}`.
  Both modes must be integer or fixed-point modes and :samp:`{n}` must be twice
  the size of :samp:`{m}`.

  In other words, ``msubmn4`` is like
  ``mulmn3`` except that it also subtracts the result
  from operand 3.

  These instructions are not allowed to ``FAIL``.

  .. index:: umsubmn4 instruction pattern

umsubmn4
  Like ``msubmn4``, but zero-extend the multiplication
  operands instead of sign-extending them.

  .. index:: ssmsubmn4 instruction pattern

ssmsubmn4
  Like ``msubmn4``, but all involved operations must be
  signed-saturating.

  .. index:: usmsubmn4 instruction pattern

usmsubmn4
  Like ``umsubmn4``, but all involved operations must be
  unsigned-saturating.

  .. index:: divmodm4 instruction pattern

divmodm4
  Signed division that produces both a quotient and a remainder.
  Operand 1 is divided by operand 2 to produce a quotient stored
  in operand 0 and a remainder stored in operand 3.

  For machines with an instruction that produces both a quotient and a
  remainder, provide a pattern for :samp:`divmod{m}4` but do not
  provide patterns for :samp:`div{m}3` and :samp:`mod{m}3`.  This
  allows optimization in the relatively common case when both the quotient
  and remainder are computed.

  If an instruction that just produces a quotient or just a remainder
  exists and is more efficient than the instruction that produces both,
  write the output routine of :samp:`divmod{m}4` to call
  ``find_reg_note`` and look for a ``REG_UNUSED`` note on the
  quotient or remainder and generate the appropriate instruction.

  .. index:: udivmodm4 instruction pattern

udivmodm4
  Similar, but does unsigned division.

  .. index:: ashlm3 instruction pattern, ssashlm3 instruction pattern, usashlm3 instruction pattern

.. _shift-patterns:

:samp:`ashl{m}3`, :samp:`ssashl{m}3`, :samp:`usashl{m}3`
  Arithmetic-shift operand 1 left by a number of bits specified by operand
  2, and store the result in operand 0.  Here :samp:`{m}` is the mode of
  operand 0 and operand 1; operand 2's mode is specified by the
  instruction pattern, and the compiler will convert the operand to that
  mode before generating the instruction.  The shift or rotate expander
  or instruction pattern should explicitly specify the mode of the operand 2,
  it should never be ``VOIDmode``.  The meaning of out-of-range shift
  counts can optionally be specified by ``TARGET_SHIFT_TRUNCATION_MASK``.
  See :ref:`target_shift_truncation_mask`.  Operand 2 is always a scalar type.

  .. index:: ashrm3 instruction pattern, lshrm3 instruction pattern, rotlm3 instruction pattern, rotrm3 instruction pattern

:samp:`ashr{m}3`, :samp:`lshr{m}3`, :samp:`rotl{m}3`, :samp:`rotr{m}3`
  Other shift and rotate instructions, analogous to the
  ``ashlm3`` instructions.  Operand 2 is always a scalar type.

  .. index:: vashlm3 instruction pattern, vashrm3 instruction pattern, vlshrm3 instruction pattern, vrotlm3 instruction pattern, vrotrm3 instruction pattern

:samp:`vashl{m}3`, :samp:`vashr{m}3`, :samp:`vlshr{m}3`, :samp:`vrotl{m}3`, :samp:`vrotr{m}3`
  Vector shift and rotate instructions that take vectors as operand 2
  instead of a scalar type.

  .. index:: avgm3_floor instruction pattern, uavgm3_floor instruction pattern

avgm3_floor uavgm3_floor
  Signed and unsigned average instructions.  These instructions add
  operands 1 and 2 without truncation, divide the result by 2,
  round towards -Inf, and store the result in operand 0.  This is
  equivalent to the C code:

  .. code-block:: c++

    narrow op0, op1, op2;
    ...
    op0 = (narrow) (((wide) op1 + (wide) op2) >> 1);

  where the sign of :samp:`narrow` determines whether this is a signed
  or unsigned operation.

  .. index:: avgm3_ceil instruction pattern, uavgm3_ceil instruction pattern

avgm3_ceil uavgm3_ceil
  Like :samp:`avg{m}3_floor` and :samp:`uavg{m}3_floor`, but round
  towards +Inf.  This is equivalent to the C code:

  .. code-block:: c++

    narrow op0, op1, op2;
    ...
    op0 = (narrow) (((wide) op1 + (wide) op2 + 1) >> 1);

  .. index:: bswapm2 instruction pattern

bswapm2
  Reverse the order of bytes of operand 1 and store the result in operand 0.

  .. index:: negm2 instruction pattern, ssnegm2 instruction pattern, usnegm2 instruction pattern

:samp:`neg{m}2`, :samp:`ssneg{m}2`, :samp:`usneg{m}2`
  Negate operand 1 and store the result in operand 0.

  .. index:: negvm3 instruction pattern

negvm3
  Like ``negm2`` but takes a ``code_label`` as operand 2 and
  emits code to jump to it if signed overflow occurs during the negation.

  .. index:: absm2 instruction pattern

absm2
  Store the absolute value of operand 1 into operand 0.

  .. index:: sqrtm2 instruction pattern

sqrtm2
  Store the square root of operand 1 into operand 0.  Both operands have
  mode :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: rsqrtm2 instruction pattern

rsqrtm2
  Store the reciprocal of the square root of operand 1 into operand 0.
  Both operands have mode :samp:`{m}`, which is a scalar or vector
  floating-point mode.

  On most architectures this pattern is only approximate, so either
  its C condition or the ``TARGET_OPTAB_SUPPORTED_P`` hook should
  check for the appropriate math flags.  (Using the C condition is
  more direct, but using ``TARGET_OPTAB_SUPPORTED_P`` can be useful
  if a target-specific built-in also uses the :samp:`rsqrt{m}2`
  pattern.)

  This pattern is not allowed to ``FAIL``.

  .. index:: fmodm3 instruction pattern

fmodm3
  Store the remainder of dividing operand 1 by operand 2 into
  operand 0, rounded towards zero to an integer.  All operands have
  mode :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: remainderm3 instruction pattern

remainderm3
  Store the remainder of dividing operand 1 by operand 2 into
  operand 0, rounded to the nearest integer.  All operands have
  mode :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: scalbm3 instruction pattern

scalbm3
  Raise ``FLT_RADIX`` to the power of operand 2, multiply it by
  operand 1, and store the result in operand 0.  All operands have
  mode :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: ldexpm3 instruction pattern

ldexpm3
  Raise 2 to the power of operand 2, multiply it by operand 1, and store
  the result in operand 0.  Operands 0 and 1 have mode :samp:`{m}`, which is
  a scalar or vector floating-point mode.  Operand 2's mode has
  the same number of elements as :samp:`{m}` and each element is wide
  enough to store an ``int``.  The integers are signed.

  This pattern is not allowed to ``FAIL``.

  .. index:: cosm2 instruction pattern

cosm2
  Store the cosine of operand 1 into operand 0.  Both operands have
  mode :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: sinm2 instruction pattern

sinm2
  Store the sine of operand 1 into operand 0.  Both operands have
  mode :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: sincosm3 instruction pattern

sincosm3
  Store the cosine of operand 2 into operand 0 and the sine of
  operand 2 into operand 1.  All operands have mode :samp:`{m}`,
  which is a scalar or vector floating-point mode.

  Targets that can calculate the sine and cosine simultaneously can
  implement this pattern as opposed to implementing individual
  ``sinm2`` and ``cosm2`` patterns.  The ``sin``
  and ``cos`` built-in functions will then be expanded to the
  ``sincosm3`` pattern, with one of the output values
  left unused.

  .. index:: tanm2 instruction pattern

tanm2
  Store the tangent of operand 1 into operand 0.  Both operands have
  mode :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: asinm2 instruction pattern

asinm2
  Store the arc sine of operand 1 into operand 0.  Both operands have
  mode :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: acosm2 instruction pattern

acosm2
  Store the arc cosine of operand 1 into operand 0.  Both operands have
  mode :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: atanm2 instruction pattern

atanm2
  Store the arc tangent of operand 1 into operand 0.  Both operands have
  mode :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: fegetroundm instruction pattern

fegetroundm
  Store the current machine floating-point rounding mode into operand 0.
  Operand 0 has mode :samp:`{m}`, which is scalar.  This pattern is used to
  implement the ``fegetround`` function from the ISO C99 standard.

  .. index:: feclearexceptm instruction pattern, feraiseexceptm instruction pattern

  feclearexceptm
feraiseexceptm
  Clears or raises the supported machine floating-point exceptions
  represented by the bits in operand 1.  Error status is stored as
  nonzero value in operand 0.  Both operands have mode :samp:`{m}`, which is
  a scalar.  These patterns are used to implement the
  ``feclearexcept`` and ``feraiseexcept`` functions from the ISO
  C99 standard.

  .. index:: expm2 instruction pattern

expm2
  Raise e (the base of natural logarithms) to the power of operand 1
  and store the result in operand 0.  Both operands have mode :samp:`{m}`,
  which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: expm1m2 instruction pattern

expm1m2
  Raise e (the base of natural logarithms) to the power of operand 1,
  subtract 1, and store the result in operand 0.  Both operands have
  mode :samp:`{m}`, which is a scalar or vector floating-point mode.

  For inputs close to zero, the pattern is expected to be more
  accurate than a separate ``expm2`` and ``subm3``
  would be.

  This pattern is not allowed to ``FAIL``.

  .. index:: exp10m2 instruction pattern

exp10m2
  Raise 10 to the power of operand 1 and store the result in operand 0.
  Both operands have mode :samp:`{m}`, which is a scalar or vector
  floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: exp2m2 instruction pattern

exp2m2
  Raise 2 to the power of operand 1 and store the result in operand 0.
  Both operands have mode :samp:`{m}`, which is a scalar or vector
  floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: logm2 instruction pattern

logm2
  Store the natural logarithm of operand 1 into operand 0.  Both operands
  have mode :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: log1pm2 instruction pattern

log1pm2
  Add 1 to operand 1, compute the natural logarithm, and store
  the result in operand 0.  Both operands have mode :samp:`{m}`, which is
  a scalar or vector floating-point mode.

  For inputs close to zero, the pattern is expected to be more
  accurate than a separate ``addm3`` and ``logm2``
  would be.

  This pattern is not allowed to ``FAIL``.

  .. index:: log10m2 instruction pattern

log10m2
  Store the base-10 logarithm of operand 1 into operand 0.  Both operands
  have mode :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: log2m2 instruction pattern

log2m2
  Store the base-2 logarithm of operand 1 into operand 0.  Both operands
  have mode :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: logbm2 instruction pattern

logbm2
  Store the base- ``FLT_RADIX`` logarithm of operand 1 into operand 0.
  Both operands have mode :samp:`{m}`, which is a scalar or vector
  floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: significandm2 instruction pattern

significandm2
  Store the significand of floating-point operand 1 in operand 0.
  Both operands have mode :samp:`{m}`, which is a scalar or vector
  floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: powm3 instruction pattern

powm3
  Store the value of operand 1 raised to the exponent operand 2
  into operand 0.  All operands have mode :samp:`{m}`, which is a scalar
  or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: atan2m3 instruction pattern

atan2m3
  Store the arc tangent (inverse tangent) of operand 1 divided by
  operand 2 into operand 0, using the signs of both arguments to
  determine the quadrant of the result.  All operands have mode
  :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: floorm2 instruction pattern

floorm2
  Store the largest integral value not greater than operand 1 in operand 0.
  Both operands have mode :samp:`{m}`, which is a scalar or vector
  floating-point mode.  If :option:`-ffp-int-builtin-inexact` is in
  effect, the 'inexact' exception may be raised for noninteger
  operands; otherwise, it may not.

  This pattern is not allowed to ``FAIL``.

  .. index:: btruncm2 instruction pattern

btruncm2
  Round operand 1 to an integer, towards zero, and store the result in
  operand 0.  Both operands have mode :samp:`{m}`, which is a scalar or
  vector floating-point mode.  If :option:`-ffp-int-builtin-inexact` is
  in effect, the 'inexact' exception may be raised for noninteger
  operands; otherwise, it may not.

  This pattern is not allowed to ``FAIL``.

  .. index:: roundm2 instruction pattern

roundm2
  Round operand 1 to the nearest integer, rounding away from zero in the
  event of a tie, and store the result in operand 0.  Both operands have
  mode :samp:`{m}`, which is a scalar or vector floating-point mode.  If
  :option:`-ffp-int-builtin-inexact` is in effect, the 'inexact'
  exception may be raised for noninteger operands; otherwise, it may
  not.

  This pattern is not allowed to ``FAIL``.

  .. index:: ceilm2 instruction pattern

ceilm2
  Store the smallest integral value not less than operand 1 in operand 0.
  Both operands have mode :samp:`{m}`, which is a scalar or vector
  floating-point mode.  If :option:`-ffp-int-builtin-inexact` is in
  effect, the 'inexact' exception may be raised for noninteger
  operands; otherwise, it may not.

  This pattern is not allowed to ``FAIL``.

  .. index:: nearbyintm2 instruction pattern

nearbyintm2
  Round operand 1 to an integer, using the current rounding mode, and
  store the result in operand 0.  Do not raise an inexact condition when
  the result is different from the argument.  Both operands have mode
  :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: rintm2 instruction pattern

rintm2
  Round operand 1 to an integer, using the current rounding mode, and
  store the result in operand 0.  Raise an inexact condition when
  the result is different from the argument.  Both operands have mode
  :samp:`{m}`, which is a scalar or vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: lrintmn2

lrintmn2
  Convert operand 1 (valid for floating point mode :samp:`{m}`) to fixed
  point mode :samp:`{n}` as a signed number according to the current
  rounding mode and store in operand 0 (which has mode :samp:`{n}`).

  .. index:: lroundmn2

lroundmn2
  Convert operand 1 (valid for floating point mode :samp:`{m}`) to fixed
  point mode :samp:`{n}` as a signed number rounding to nearest and away
  from zero and store in operand 0 (which has mode :samp:`{n}`).

  .. index:: lfloormn2

lfloormn2
  Convert operand 1 (valid for floating point mode :samp:`{m}`) to fixed
  point mode :samp:`{n}` as a signed number rounding down and store in
  operand 0 (which has mode :samp:`{n}`).

  .. index:: lceilmn2

lceilmn2
  Convert operand 1 (valid for floating point mode :samp:`{m}`) to fixed
  point mode :samp:`{n}` as a signed number rounding up and store in
  operand 0 (which has mode :samp:`{n}`).

  .. index:: copysignm3 instruction pattern

copysignm3
  Store a value with the magnitude of operand 1 and the sign of operand
  2 into operand 0.  All operands have mode :samp:`{m}`, which is a scalar or
  vector floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: xorsignm3 instruction pattern

xorsignm3
  Equivalent to :samp:`op0 = op1 * copysign (1.0, op2)`: store a value with
  the magnitude of operand 1 and the sign of operand 2 into operand 0.
  All operands have mode :samp:`{m}`, which is a scalar or vector
  floating-point mode.

  This pattern is not allowed to ``FAIL``.

  .. index:: issignalingm2 instruction pattern

issignalingm2
  Set operand 0 to 1 if operand 1 is a signaling NaN and to 0 otherwise.

  .. index:: cadd90m3 instruction pattern

cadd90m3
  Perform vector add and subtract on even/odd number pairs.  The operation being
  matched is semantically described as

  .. code-block:: c++

      for (int i = 0; i < N; i += 2)
        {
          c[i] = a[i] - b[i+1];
          c[i+1] = a[i+1] + b[i];
        }

  This operation is semantically equivalent to performing a vector addition of
  complex numbers in operand 1 with operand 2 rotated by 90 degrees around
  the argand plane and storing the result in operand 0.

  In GCC lane ordering the real part of the number must be in the even lanes with
  the imaginary part in the odd lanes.

  The operation is only supported for vector modes :samp:`{m}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: cadd270m3 instruction pattern

cadd270m3
  Perform vector add and subtract on even/odd number pairs.  The operation being
  matched is semantically described as

  .. code-block:: c++

      for (int i = 0; i < N; i += 2)
        {
          c[i] = a[i] + b[i+1];
          c[i+1] = a[i+1] - b[i];
        }

  This operation is semantically equivalent to performing a vector addition of
  complex numbers in operand 1 with operand 2 rotated by 270 degrees around
  the argand plane and storing the result in operand 0.

  In GCC lane ordering the real part of the number must be in the even lanes with
  the imaginary part in the odd lanes.

  The operation is only supported for vector modes :samp:`{m}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: cmlam4 instruction pattern

cmlam4
  Perform a vector multiply and accumulate that is semantically the same as
  a multiply and accumulate of complex numbers.

  .. code-block:: c++

      complex TYPE op0[N];
      complex TYPE op1[N];
      complex TYPE op2[N];
      complex TYPE op3[N];
      for (int i = 0; i < N; i += 1)
        {
          op0[i] = op1[i] * op2[i] + op3[i];
        }

  In GCC lane ordering the real part of the number must be in the even lanes with
  the imaginary part in the odd lanes.

  The operation is only supported for vector modes :samp:`{m}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: cmla_conjm4 instruction pattern

cmla_conjm4
  Perform a vector multiply by conjugate and accumulate that is semantically
  the same as a multiply and accumulate of complex numbers where the second
  multiply arguments is conjugated.

  .. code-block:: c++

      complex TYPE op0[N];
      complex TYPE op1[N];
      complex TYPE op2[N];
      complex TYPE op3[N];
      for (int i = 0; i < N; i += 1)
        {
          op0[i] = op1[i] * conj (op2[i]) + op3[i];
        }

  In GCC lane ordering the real part of the number must be in the even lanes with
  the imaginary part in the odd lanes.

  The operation is only supported for vector modes :samp:`{m}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: cmlsm4 instruction pattern

cmlsm4
  Perform a vector multiply and subtract that is semantically the same as
  a multiply and subtract of complex numbers.

  .. code-block:: c++

      complex TYPE op0[N];
      complex TYPE op1[N];
      complex TYPE op2[N];
      complex TYPE op3[N];
      for (int i = 0; i < N; i += 1)
        {
          op0[i] = op1[i] * op2[i] - op3[i];
        }

  In GCC lane ordering the real part of the number must be in the even lanes with
  the imaginary part in the odd lanes.

  The operation is only supported for vector modes :samp:`{m}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: cmls_conjm4 instruction pattern

cmls_conjm4
  Perform a vector multiply by conjugate and subtract that is semantically
  the same as a multiply and subtract of complex numbers where the second
  multiply arguments is conjugated.

  .. code-block:: c++

      complex TYPE op0[N];
      complex TYPE op1[N];
      complex TYPE op2[N];
      complex TYPE op3[N];
      for (int i = 0; i < N; i += 1)
        {
          op0[i] = op1[i] * conj (op2[i]) - op3[i];
        }

  In GCC lane ordering the real part of the number must be in the even lanes with
  the imaginary part in the odd lanes.

  The operation is only supported for vector modes :samp:`{m}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: cmulm4 instruction pattern

cmulm4
  Perform a vector multiply that is semantically the same as multiply of
  complex numbers.

  .. code-block:: c++

      complex TYPE op0[N];
      complex TYPE op1[N];
      complex TYPE op2[N];
      for (int i = 0; i < N; i += 1)
        {
          op0[i] = op1[i] * op2[i];
        }

  In GCC lane ordering the real part of the number must be in the even lanes with
  the imaginary part in the odd lanes.

  The operation is only supported for vector modes :samp:`{m}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: cmul_conjm4 instruction pattern

cmul_conjm4
  Perform a vector multiply by conjugate that is semantically the same as a
  multiply of complex numbers where the second multiply arguments is conjugated.

  .. code-block:: c++

      complex TYPE op0[N];
      complex TYPE op1[N];
      complex TYPE op2[N];
      for (int i = 0; i < N; i += 1)
        {
          op0[i] = op1[i] * conj (op2[i]);
        }

  In GCC lane ordering the real part of the number must be in the even lanes with
  the imaginary part in the odd lanes.

  The operation is only supported for vector modes :samp:`{m}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: ffsm2 instruction pattern

ffsm2
  Store into operand 0 one plus the index of the least significant 1-bit
  of operand 1.  If operand 1 is zero, store zero.

  :samp:`{m}` is either a scalar or vector integer mode.  When it is a scalar,
  operand 1 has mode :samp:`{m}` but operand 0 can have whatever scalar
  integer mode is suitable for the target.  The compiler will insert
  conversion instructions as necessary (typically to convert the result
  to the same width as ``int``).  When :samp:`{m}` is a vector, both
  operands must have mode :samp:`{m}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: clrsbm2 instruction pattern

clrsbm2
  Count leading redundant sign bits.
  Store into operand 0 the number of redundant sign bits in operand 1, starting
  at the most significant bit position.
  A redundant sign bit is defined as any sign bit after the first. As such,
  this count will be one less than the count of leading sign bits.

  :samp:`{m}` is either a scalar or vector integer mode.  When it is a scalar,
  operand 1 has mode :samp:`{m}` but operand 0 can have whatever scalar
  integer mode is suitable for the target.  The compiler will insert
  conversion instructions as necessary (typically to convert the result
  to the same width as ``int``).  When :samp:`{m}` is a vector, both
  operands must have mode :samp:`{m}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: clzm2 instruction pattern

clzm2
  Store into operand 0 the number of leading 0-bits in operand 1, starting
  at the most significant bit position.  If operand 1 is 0, the
  ``CLZ_DEFINED_VALUE_AT_ZERO`` (see :ref:`misc`) macro defines if
  the result is undefined or has a useful value.

  :samp:`{m}` is either a scalar or vector integer mode.  When it is a scalar,
  operand 1 has mode :samp:`{m}` but operand 0 can have whatever scalar
  integer mode is suitable for the target.  The compiler will insert
  conversion instructions as necessary (typically to convert the result
  to the same width as ``int``).  When :samp:`{m}` is a vector, both
  operands must have mode :samp:`{m}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: ctzm2 instruction pattern

ctzm2
  Store into operand 0 the number of trailing 0-bits in operand 1, starting
  at the least significant bit position.  If operand 1 is 0, the
  ``CTZ_DEFINED_VALUE_AT_ZERO`` (see :ref:`misc`) macro defines if
  the result is undefined or has a useful value.

  :samp:`{m}` is either a scalar or vector integer mode.  When it is a scalar,
  operand 1 has mode :samp:`{m}` but operand 0 can have whatever scalar
  integer mode is suitable for the target.  The compiler will insert
  conversion instructions as necessary (typically to convert the result
  to the same width as ``int``).  When :samp:`{m}` is a vector, both
  operands must have mode :samp:`{m}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: popcountm2 instruction pattern

popcountm2
  Store into operand 0 the number of 1-bits in operand 1.

  :samp:`{m}` is either a scalar or vector integer mode.  When it is a scalar,
  operand 1 has mode :samp:`{m}` but operand 0 can have whatever scalar
  integer mode is suitable for the target.  The compiler will insert
  conversion instructions as necessary (typically to convert the result
  to the same width as ``int``).  When :samp:`{m}` is a vector, both
  operands must have mode :samp:`{m}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: paritym2 instruction pattern

paritym2
  Store into operand 0 the parity of operand 1, i.e. the number of 1-bits
  in operand 1 modulo 2.

  :samp:`{m}` is either a scalar or vector integer mode.  When it is a scalar,
  operand 1 has mode :samp:`{m}` but operand 0 can have whatever scalar
  integer mode is suitable for the target.  The compiler will insert
  conversion instructions as necessary (typically to convert the result
  to the same width as ``int``).  When :samp:`{m}` is a vector, both
  operands must have mode :samp:`{m}`.

  This pattern is not allowed to ``FAIL``.

  .. index:: one_cmplm2 instruction pattern

one_cmplm2
  Store the bitwise-complement of operand 1 into operand 0.

  .. index:: cpymemm instruction pattern

cpymemm
  Block copy instruction.  The destination and source blocks of memory
  are the first two operands, and both are ``mem:BLK`` s with an
  address in mode ``Pmode``.

  The number of bytes to copy is the third operand, in mode :samp:`{m}`.
  Usually, you specify ``Pmode`` for :samp:`{m}`.  However, if you can
  generate better code knowing the range of valid lengths is smaller than
  those representable in a full Pmode pointer, you should provide
  a pattern with a
  mode corresponding to the range of values you can handle efficiently
  (e.g., ``QImode`` for values in the range 0--127; note we avoid numbers
  that appear negative) and also a pattern with ``Pmode``.

  The fourth operand is the known shared alignment of the source and
  destination, in the form of a ``const_int`` rtx.  Thus, if the
  compiler knows that both source and destination are word-aligned,
  it may provide the value 4 for this operand.

  Optional operands 5 and 6 specify expected alignment and size of block
  respectively.  The expected alignment differs from alignment in operand 4
  in a way that the blocks are not required to be aligned according to it in
  all cases. This expected alignment is also in bytes, just like operand 4.
  Expected size, when unknown, is set to ``(const_int -1)``.

  Descriptions of multiple ``cpymemm`` patterns can only be
  beneficial if the patterns for smaller modes have fewer restrictions
  on their first, second and fourth operands.  Note that the mode :samp:`{m}`
  in ``cpymemm`` does not impose any restriction on the mode of
  individually copied data units in the block.

  The ``cpymemm`` patterns need not give special consideration
  to the possibility that the source and destination strings might
  overlap. These patterns are used to do inline expansion of
  ``__builtin_memcpy``.

  .. index:: movmemm instruction pattern

movmemm
  Block move instruction.  The destination and source blocks of memory
  are the first two operands, and both are ``mem:BLK`` s with an
  address in mode ``Pmode``.

  The number of bytes to copy is the third operand, in mode :samp:`{m}`.
  Usually, you specify ``Pmode`` for :samp:`{m}`.  However, if you can
  generate better code knowing the range of valid lengths is smaller than
  those representable in a full Pmode pointer, you should provide
  a pattern with a
  mode corresponding to the range of values you can handle efficiently
  (e.g., ``QImode`` for values in the range 0--127; note we avoid numbers
  that appear negative) and also a pattern with ``Pmode``.

  The fourth operand is the known shared alignment of the source and
  destination, in the form of a ``const_int`` rtx.  Thus, if the
  compiler knows that both source and destination are word-aligned,
  it may provide the value 4 for this operand.

  Optional operands 5 and 6 specify expected alignment and size of block
  respectively.  The expected alignment differs from alignment in operand 4
  in a way that the blocks are not required to be aligned according to it in
  all cases. This expected alignment is also in bytes, just like operand 4.
  Expected size, when unknown, is set to ``(const_int -1)``.

  Descriptions of multiple ``movmemm`` patterns can only be
  beneficial if the patterns for smaller modes have fewer restrictions
  on their first, second and fourth operands.  Note that the mode :samp:`{m}`
  in ``movmemm`` does not impose any restriction on the mode of
  individually copied data units in the block.

  The ``movmemm`` patterns must correctly handle the case where
  the source and destination strings overlap. These patterns are used to
  do inline expansion of ``__builtin_memmove``.

  .. index:: movstr instruction pattern

movstr
  String copy instruction, with ``stpcpy`` semantics.  Operand 0 is
  an output operand in mode ``Pmode``.  The addresses of the
  destination and source strings are operands 1 and 2, and both are
  ``mem:BLK`` s with addresses in mode ``Pmode``.  The execution of
  the expansion of this pattern should store in operand 0 the address in
  which the ``NUL`` terminator was stored in the destination string.

  This pattern has also several optional operands that are same as in
  ``setmem``.

  .. index:: setmemm instruction pattern

setmemm
  Block set instruction.  The destination string is the first operand,
  given as a ``mem:BLK`` whose address is in mode ``Pmode``.  The
  number of bytes to set is the second operand, in mode :samp:`{m}`.  The value to
  initialize the memory with is the third operand. Targets that only support the
  clearing of memory should reject any value that is not the constant 0.  See
  :samp:`cpymem{m}` for a discussion of the choice of mode.

  The fourth operand is the known alignment of the destination, in the form
  of a ``const_int`` rtx.  Thus, if the compiler knows that the
  destination is word-aligned, it may provide the value 4 for this
  operand.

  Optional operands 5 and 6 specify expected alignment and size of block
  respectively.  The expected alignment differs from alignment in operand 4
  in a way that the blocks are not required to be aligned according to it in
  all cases. This expected alignment is also in bytes, just like operand 4.
  Expected size, when unknown, is set to ``(const_int -1)``.
  Operand 7 is the minimal size of the block and operand 8 is the
  maximal size of the block (NULL if it cannot be represented as CONST_INT).
  Operand 9 is the probable maximal size (i.e. we cannot rely on it for
  correctness, but it can be used for choosing proper code sequence for a
  given size).

  The use for multiple ``setmemm`` is as for ``cpymemm``.

  .. index:: cmpstrnm instruction pattern

cmpstrnm
  String compare instruction, with five operands.  Operand 0 is the output;
  it has mode :samp:`{m}`.  The remaining four operands are like the operands
  of :samp:`cpymem{m}`.  The two memory blocks specified are compared
  byte by byte in lexicographic order starting at the beginning of each
  string.  The instruction is not allowed to prefetch more than one byte
  at a time since either string may end in the first byte and reading past
  that may access an invalid page or segment and cause a fault.  The
  comparison terminates early if the fetched bytes are different or if
  they are equal to zero.  The effect of the instruction is to store a
  value in operand 0 whose sign indicates the result of the comparison.

  .. index:: cmpstrm instruction pattern

cmpstrm
  String compare instruction, without known maximum length.  Operand 0 is the
  output; it has mode :samp:`{m}`.  The second and third operand are the blocks of
  memory to be compared; both are ``mem:BLK`` with an address in mode
  ``Pmode``.

  The fourth operand is the known shared alignment of the source and
  destination, in the form of a ``const_int`` rtx.  Thus, if the
  compiler knows that both source and destination are word-aligned,
  it may provide the value 4 for this operand.

  The two memory blocks specified are compared byte by byte in lexicographic
  order starting at the beginning of each string.  The instruction is not allowed
  to prefetch more than one byte at a time since either string may end in the
  first byte and reading past that may access an invalid page or segment and
  cause a fault.  The comparison will terminate when the fetched bytes
  are different or if they are equal to zero.  The effect of the
  instruction is to store a value in operand 0 whose sign indicates the
  result of the comparison.

  .. index:: cmpmemm instruction pattern

cmpmemm
  Block compare instruction, with five operands like the operands
  of :samp:`cmpstr{m}`.  The two memory blocks specified are compared
  byte by byte in lexicographic order starting at the beginning of each
  block.  Unlike :samp:`cmpstr{m}` the instruction can prefetch
  any bytes in the two memory blocks.  Also unlike :samp:`cmpstr{m}`
  the comparison will not stop if both bytes are zero.  The effect of
  the instruction is to store a value in operand 0 whose sign indicates
  the result of the comparison.

  .. index:: strlenm instruction pattern

strlenm
  Compute the length of a string, with three operands.
  Operand 0 is the result (of mode :samp:`{m}`), operand 1 is
  a ``mem`` referring to the first character of the string,
  operand 2 is the character to search for (normally zero),
  and operand 3 is a constant describing the known alignment
  of the beginning of the string.

  .. index:: rawmemchrm instruction pattern

rawmemchrm
  Scan memory referred to by operand 1 for the first occurrence of operand 2.
  Operand 1 is a ``mem`` and operand 2 a ``const_int`` of mode :samp:`{m}`.
  Operand 0 is the result, i.e., a pointer to the first occurrence of operand 2
  in the memory block given by operand 1.

  .. index:: floatmn2 instruction pattern

floatmn2
  Convert signed integer operand 1 (valid for fixed point mode :samp:`{m}`) to
  floating point mode :samp:`{n}` and store in operand 0 (which has mode
  :samp:`{n}`).

  .. index:: floatunsmn2 instruction pattern

floatunsmn2
  Convert unsigned integer operand 1 (valid for fixed point mode :samp:`{m}`)
  to floating point mode :samp:`{n}` and store in operand 0 (which has mode
  :samp:`{n}`).

  .. index:: fixmn2 instruction pattern

fixmn2
  Convert operand 1 (valid for floating point mode :samp:`{m}`) to fixed
  point mode :samp:`{n}` as a signed number and store in operand 0 (which
  has mode :samp:`{n}`).  This instruction's result is defined only when
  the value of operand 1 is an integer.

  If the machine description defines this pattern, it also needs to
  define the ``ftrunc`` pattern.

  .. index:: fixunsmn2 instruction pattern

fixunsmn2
  Convert operand 1 (valid for floating point mode :samp:`{m}`) to fixed
  point mode :samp:`{n}` as an unsigned number and store in operand 0 (which
  has mode :samp:`{n}`).  This instruction's result is defined only when the
  value of operand 1 is an integer.

  .. index:: ftruncm2 instruction pattern

ftruncm2
  Convert operand 1 (valid for floating point mode :samp:`{m}`) to an
  integer value, still represented in floating point mode :samp:`{m}`, and
  store it in operand 0 (valid for floating point mode :samp:`{m}`).

  .. index:: fix_truncmn2 instruction pattern

fix_truncmn2
  Like :samp:`fix{m}{n}2` but works for any floating point value
  of mode :samp:`{m}` by converting the value to an integer.

  .. index:: fixuns_truncmn2 instruction pattern

fixuns_truncmn2
  Like :samp:`fixuns{m}{n}2` but works for any floating point
  value of mode :samp:`{m}` by converting the value to an integer.

  .. index:: truncmn2 instruction pattern

truncmn2
  Truncate operand 1 (valid for mode :samp:`{m}`) to mode :samp:`{n}` and
  store in operand 0 (which has mode :samp:`{n}`).  Both modes must be fixed
  point or both floating point.

  .. index:: extendmn2 instruction pattern

extendmn2
  Sign-extend operand 1 (valid for mode :samp:`{m}`) to mode :samp:`{n}` and
  store in operand 0 (which has mode :samp:`{n}`).  Both modes must be fixed
  point or both floating point.

  .. index:: zero_extendmn2 instruction pattern

zero_extendmn2
  Zero-extend operand 1 (valid for mode :samp:`{m}`) to mode :samp:`{n}` and
  store in operand 0 (which has mode :samp:`{n}`).  Both modes must be fixed
  point.

  .. index:: fractmn2 instruction pattern

fractmn2
  Convert operand 1 of mode :samp:`{m}` to mode :samp:`{n}` and store in
  operand 0 (which has mode :samp:`{n}`).  Mode :samp:`{m}` and mode :samp:`{n}`
  could be fixed-point to fixed-point, signed integer to fixed-point,
  fixed-point to signed integer, floating-point to fixed-point,
  or fixed-point to floating-point.
  When overflows or underflows happen, the results are undefined.

  .. index:: satfractmn2 instruction pattern

satfractmn2
  Convert operand 1 of mode :samp:`{m}` to mode :samp:`{n}` and store in
  operand 0 (which has mode :samp:`{n}`).  Mode :samp:`{m}` and mode :samp:`{n}`
  could be fixed-point to fixed-point, signed integer to fixed-point,
  or floating-point to fixed-point.
  When overflows or underflows happen, the instruction saturates the
  results to the maximum or the minimum.

  .. index:: fractunsmn2 instruction pattern

fractunsmn2
  Convert operand 1 of mode :samp:`{m}` to mode :samp:`{n}` and store in
  operand 0 (which has mode :samp:`{n}`).  Mode :samp:`{m}` and mode :samp:`{n}`
  could be unsigned integer to fixed-point, or
  fixed-point to unsigned integer.
  When overflows or underflows happen, the results are undefined.

  .. index:: satfractunsmn2 instruction pattern

satfractunsmn2
  Convert unsigned integer operand 1 of mode :samp:`{m}` to fixed-point mode
  :samp:`{n}` and store in operand 0 (which has mode :samp:`{n}`).
  When overflows or underflows happen, the instruction saturates the
  results to the maximum or the minimum.

  .. index:: extvm instruction pattern

extvm
  Extract a bit-field from register operand 1, sign-extend it, and store
  it in operand 0.  Operand 2 specifies the width of the field in bits
  and operand 3 the starting bit, which counts from the most significant
  bit if :samp:`BITS_BIG_ENDIAN` is true and from the least significant bit
  otherwise.

  Operands 0 and 1 both have mode :samp:`{m}`.  Operands 2 and 3 have a
  target-specific mode.

  .. index:: extvmisalignm instruction pattern

extvmisalignm
  Extract a bit-field from memory operand 1, sign extend it, and store
  it in operand 0.  Operand 2 specifies the width in bits and operand 3
  the starting bit.  The starting bit is always somewhere in the first byte of
  operand 1; it counts from the most significant bit if :samp:`BITS_BIG_ENDIAN`
  is true and from the least significant bit otherwise.

  Operand 0 has mode :samp:`{m}` while operand 1 has ``BLK`` mode.
  Operands 2 and 3 have a target-specific mode.

  The instruction must not read beyond the last byte of the bit-field.

  .. index:: extzvm instruction pattern

extzvm
  Like :samp:`extv{m}` except that the bit-field value is zero-extended.

  .. index:: extzvmisalignm instruction pattern

extzvmisalignm
  Like :samp:`extvmisalign{m}` except that the bit-field value is
  zero-extended.

  .. index:: insvm instruction pattern

insvm
  Insert operand 3 into a bit-field of register operand 0.  Operand 1
  specifies the width of the field in bits and operand 2 the starting bit,
  which counts from the most significant bit if :samp:`BITS_BIG_ENDIAN`
  is true and from the least significant bit otherwise.

  Operands 0 and 3 both have mode :samp:`{m}`.  Operands 1 and 2 have a
  target-specific mode.

  .. index:: insvmisalignm instruction pattern

insvmisalignm
  Insert operand 3 into a bit-field of memory operand 0.  Operand 1
  specifies the width of the field in bits and operand 2 the starting bit.
  The starting bit is always somewhere in the first byte of operand 0;
  it counts from the most significant bit if :samp:`BITS_BIG_ENDIAN`
  is true and from the least significant bit otherwise.

  Operand 3 has mode :samp:`{m}` while operand 0 has ``BLK`` mode.
  Operands 1 and 2 have a target-specific mode.

  The instruction must not read or write beyond the last byte of the bit-field.

  .. index:: extv instruction pattern

extv
  Extract a bit-field from operand 1 (a register or memory operand), where
  operand 2 specifies the width in bits and operand 3 the starting bit,
  and store it in operand 0.  Operand 0 must have mode ``word_mode``.
  Operand 1 may have mode ``byte_mode`` or ``word_mode`` ; often
  ``word_mode`` is allowed only for registers.  Operands 2 and 3 must
  be valid for ``word_mode``.

  The RTL generation pass generates this instruction only with constants
  for operands 2 and 3 and the constant is never zero for operand 2.

  The bit-field value is sign-extended to a full word integer
  before it is stored in operand 0.

  This pattern is deprecated; please use :samp:`extv{m}` and
  ``extvmisalignm`` instead.

  .. index:: extzv instruction pattern

extzv
  Like :samp:`extv` except that the bit-field value is zero-extended.

  This pattern is deprecated; please use :samp:`extzv{m}` and
  ``extzvmisalignm`` instead.

  .. index:: insv instruction pattern

insv
  Store operand 3 (which must be valid for ``word_mode``) into a
  bit-field in operand 0, where operand 1 specifies the width in bits and
  operand 2 the starting bit.  Operand 0 may have mode ``byte_mode`` or
  ``word_mode`` ; often ``word_mode`` is allowed only for registers.
  Operands 1 and 2 must be valid for ``word_mode``.

  The RTL generation pass generates this instruction only with constants
  for operands 1 and 2 and the constant is never zero for operand 1.

  This pattern is deprecated; please use :samp:`insv{m}` and
  ``insvmisalignm`` instead.

  .. index:: movmodecc instruction pattern

movmodecc
  Conditionally move operand 2 or operand 3 into operand 0 according to the
  comparison in operand 1.  If the comparison is true, operand 2 is moved
  into operand 0, otherwise operand 3 is moved.

  The mode of the operands being compared need not be the same as the operands
  being moved.  Some machines, sparc64 for example, have instructions that
  conditionally move an integer value based on the floating point condition
  codes and vice versa.

  If the machine does not have conditional move instructions, do not
  define these patterns.

  .. index:: addmodecc instruction pattern

addmodecc
  Similar to :samp:`mov{mode}cc` but for conditional addition.  Conditionally
  move operand 2 or (operands 2 + operand 3) into operand 0 according to the
  comparison in operand 1.  If the comparison is false, operand 2 is moved into
  operand 0, otherwise (operand 2 + operand 3) is moved.

  .. index:: cond_addmode instruction pattern, cond_submode instruction pattern, cond_mulmode instruction pattern, cond_divmode instruction pattern, cond_udivmode instruction pattern, cond_modmode instruction pattern, cond_umodmode instruction pattern, cond_andmode instruction pattern, cond_iormode instruction pattern, cond_xormode instruction pattern, cond_sminmode instruction pattern, cond_smaxmode instruction pattern, cond_uminmode instruction pattern, cond_umaxmode instruction pattern, cond_fminmode instruction pattern, cond_fmaxmode instruction pattern, cond_ashlmode instruction pattern, cond_ashrmode instruction pattern, cond_lshrmode instruction pattern

cond_addmode cond_submode cond_mulmode cond_divmode cond_udivmode cond_modmode cond_umodmode cond_andmode cond_iormode cond_xormode cond_sminmode cond_smaxmode cond_uminmode cond_umaxmode cond_fminmode cond_fmaxmode cond_ashlmode cond_ashrmode cond_lshrmode
  When operand 1 is true, perform an operation on operands 2 and 3 and
  store the result in operand 0, otherwise store operand 4 in operand 0.
  The operation works elementwise if the operands are vectors.

  The scalar case is equivalent to:

  .. code-block:: c++

    op0 = op1 ? op2 op op3 : op4;

  while the vector case is equivalent to:

  .. code-block:: c++

    for (i = 0; i < GET_MODE_NUNITS (m); i++)
      op0[i] = op1[i] ? op2[i] op op3[i] : op4[i];

  where, for example, :samp:`{op}` is ``+`` for :samp:`cond_add{mode}`.

  When defined for floating-point modes, the contents of :samp:`op3[i]`
  are not interpreted if :samp:`op1[i]` is false, just like they would not
  be in a normal C :samp:`?:` condition.

  Operands 0, 2, 3 and 4 all have mode :samp:`{m}`.  Operand 1 is a scalar
  integer if :samp:`{m}` is scalar, otherwise it has the mode returned by
  ``TARGET_VECTORIZE_GET_MASK_MODE``.

  :samp:`cond_{op}{mode}` generally corresponds to a conditional
  form of :samp:`{op}{mode}3`.  As an exception, the vector forms
  of shifts correspond to patterns like ``vashlmode3`` rather
  than patterns like ``ashlmode3``.

  .. index:: cond_fmamode instruction pattern, cond_fmsmode instruction pattern, cond_fnmamode instruction pattern, cond_fnmsmode instruction pattern

cond_fmamode cond_fmsmode cond_fnmamode cond_fnmsmode
  Like :samp:`cond_add{m}`, except that the conditional operation
  takes 3 operands rather than two.  For example, the vector form of
  :samp:`cond_fma{mode}` is equivalent to:

  .. code-block:: c++

    for (i = 0; i < GET_MODE_NUNITS (m); i++)
      op0[i] = op1[i] ? fma (op2[i], op3[i], op4[i]) : op5[i];

  .. index:: negmodecc instruction pattern

negmodecc
  Similar to :samp:`mov{mode}cc` but for conditional negation.  Conditionally
  move the negation of operand 2 or the unchanged operand 3 into operand 0
  according to the comparison in operand 1.  If the comparison is true, the negation
  of operand 2 is moved into operand 0, otherwise operand 3 is moved.

  .. index:: notmodecc instruction pattern

notmodecc
  Similar to :samp:`neg{mode}cc` but for conditional complement.
  Conditionally move the bitwise complement of operand 2 or the unchanged
  operand 3 into operand 0 according to the comparison in operand 1.
  If the comparison is true, the complement of operand 2 is moved into
  operand 0, otherwise operand 3 is moved.

  .. index:: cstoremode4 instruction pattern

cstoremode4
  Store zero or nonzero in operand 0 according to whether a comparison
  is true.  Operand 1 is a comparison operator.  Operand 2 and operand 3
  are the first and second operand of the comparison, respectively.
  You specify the mode that operand 0 must have when you write the
  ``match_operand`` expression.  The compiler automatically sees which
  mode you have used and supplies an operand of that mode.

  The value stored for a true condition must have 1 as its low bit, or
  else must be negative.  Otherwise the instruction is not suitable and
  you should omit it from the machine description.  You describe to the
  compiler exactly which value is stored by defining the macro
  ``STORE_FLAG_VALUE`` (see :ref:`misc`).  If a description cannot be
  found that can be used for all the possible comparison operators, you
  should pick one and use a ``define_expand`` to map all results
  onto the one you chose.

  These operations may ``FAIL``, but should do so only in relatively
  uncommon cases; if they would ``FAIL`` for common cases involving
  integer comparisons, it is best to restrict the predicates to not
  allow these operands.  Likewise if a given comparison operator will
  always fail, independent of the operands (for floating-point modes, the
  ``ordered_comparison_operator`` predicate is often useful in this case).

  If this pattern is omitted, the compiler will generate a conditional
  branch---for example, it may copy a constant one to the target and branching
  around an assignment of zero to the target---or a libcall.  If the predicate
  for operand 1 only rejects some operators, it will also try reordering the
  operands and/or inverting the result value (e.g. by an exclusive OR).
  These possibilities could be cheaper or equivalent to the instructions
  used for the :samp:`cstore{mode}4` pattern followed by those required
  to convert a positive result from ``STORE_FLAG_VALUE`` to 1; in this
  case, you can and should make operand 1's predicate reject some operators
  in the :samp:`cstore{mode}4` pattern, or remove the pattern altogether
  from the machine description.

  .. index:: cbranchmode4 instruction pattern

cbranchmode4
  Conditional branch instruction combined with a compare instruction.
  Operand 0 is a comparison operator.  Operand 1 and operand 2 are the
  first and second operands of the comparison, respectively.  Operand 3
  is the ``code_label`` to jump to.

  .. index:: jump instruction pattern

jump
  A jump inside a function; an unconditional branch.  Operand 0 is the
  ``code_label`` to jump to.  This pattern name is mandatory on all
  machines.

  .. index:: call instruction pattern

call
  Subroutine call instruction returning no value.  Operand 0 is the
  function to call; operand 1 is the number of bytes of arguments pushed
  as a ``const_int``.  Operand 2 is the result of calling the target
  hook ``TARGET_FUNCTION_ARG`` with the second argument ``arg``
  yielding true for ``arg.end_marker_p ()``, in a call after all
  parameters have been passed to that hook.  By default this is the first
  register beyond those used for arguments in the call, or ``NULL`` if
  all the argument-registers are used in the call.

  On most machines, operand 2 is not actually stored into the RTL
  pattern.  It is supplied for the sake of some RISC machines which need
  to put this information into the assembler code; they can put it in
  the RTL instead of operand 1.

  Operand 0 should be a ``mem`` RTX whose address is the address of the
  function.  Note, however, that this address can be a ``symbol_ref``
  expression even if it would not be a legitimate memory address on the
  target machine.  If it is also not a valid argument for a call
  instruction, the pattern for this operation should be a
  ``define_expand`` (see :ref:`expander-definitions`) that places the
  address into a register and uses that register in the call instruction.

  .. index:: call_value instruction pattern

call_value
  Subroutine call instruction returning a value.  Operand 0 is the hard
  register in which the value is returned.  There are three more
  operands, the same as the three operands of the :samp:`call`
  instruction (but with numbers increased by one).

  Subroutines that return ``BLKmode`` objects use the :samp:`call`
  insn.

  .. index:: call_pop instruction pattern, call_value_pop instruction pattern

:samp:`{call_pop}, {call_value_pop}`
  Similar to :samp:`call` and :samp:`call_value`, except used if defined and
  if ``RETURN_POPS_ARGS`` is nonzero.  They should emit a ``parallel``
  that contains both the function call and a ``set`` to indicate the
  adjustment made to the frame pointer.

  For machines where ``RETURN_POPS_ARGS`` can be nonzero, the use of these
  patterns increases the number of functions for which the frame pointer
  can be eliminated, if desired.

  .. index:: untyped_call instruction pattern

untyped_call
  Subroutine call instruction returning a value of any type.  Operand 0 is
  the function to call; operand 1 is a memory location where the result of
  calling the function is to be stored; operand 2 is a ``parallel``
  expression where each element is a ``set`` expression that indicates
  the saving of a function return value into the result block.

  This instruction pattern should be defined to support
  ``__builtin_apply`` on machines where special instructions are needed
  to call a subroutine with arbitrary arguments or to save the value
  returned.  This instruction pattern is required on machines that have
  multiple registers that can hold a return value
  (i.e. ``FUNCTION_VALUE_REGNO_P`` is true for more than one register).

  .. index:: return instruction pattern

return
  Subroutine return instruction.  This instruction pattern name should be
  defined only if a single instruction can do all the work of returning
  from a function.

  Like the :samp:`mov{m}` patterns, this pattern is also used after the
  RTL generation phase.  In this case it is to support machines where
  multiple instructions are usually needed to return from a function, but
  some class of functions only requires one instruction to implement a
  return.  Normally, the applicable functions are those which do not need
  to save any registers or allocate stack space.

  It is valid for this pattern to expand to an instruction using
  ``simple_return`` if no epilogue is required.

  .. index:: simple_return instruction pattern

simple_return
  Subroutine return instruction.  This instruction pattern name should be
  defined only if a single instruction can do all the work of returning
  from a function on a path where no epilogue is required.  This pattern
  is very similar to the ``return`` instruction pattern, but it is emitted
  only by the shrink-wrapping optimization on paths where the function
  prologue has not been executed, and a function return should occur without
  any of the effects of the epilogue.  Additional uses may be introduced on
  paths where both the prologue and the epilogue have executed.

  .. index:: reload_completed, leaf_function_p

  For such machines, the condition specified in this pattern should only
  be true when ``reload_completed`` is nonzero and the function's
  epilogue would only be a single instruction.  For machines with register
  windows, the routine ``leaf_function_p`` may be used to determine if
  a register window push is required.

  Machines that have conditional return instructions should define patterns
  such as

  .. code-block::

    (define_insn ""
      [(set (pc)
            (if_then_else (match_operator
                             0 "comparison_operator"
                             [(reg:CC CC_REG) (const_int 0)])
                          (return)
                          (pc)))]
      "condition"
      "...")

  where :samp:`{condition}` would normally be the same condition specified on the
  named :samp:`return` pattern.

  .. index:: untyped_return instruction pattern

untyped_return
  Untyped subroutine return instruction.  This instruction pattern should
  be defined to support ``__builtin_return`` on machines where special
  instructions are needed to return a value of any type.

  Operand 0 is a memory location where the result of calling a function
  with ``__builtin_apply`` is stored; operand 1 is a ``parallel``
  expression where each element is a ``set`` expression that indicates
  the restoring of a function return value from the result block.

  .. index:: nop instruction pattern

nop
  No-op instruction.  This instruction pattern name should always be defined
  to output a no-op in assembler code.  ``(const_int 0)`` will do as an
  RTL pattern.

  .. index:: indirect_jump instruction pattern

indirect_jump
  An instruction to jump to an address which is operand zero.
  This pattern name is mandatory on all machines.

  .. index:: casesi instruction pattern

casesi
  Instruction to jump through a dispatch table, including bounds checking.
  This instruction takes five operands:

  * The index to dispatch on, which has mode ``SImode``.

  * The lower bound for indices in the table, an integer constant.

  * The total range of indices in the table---the largest index
    minus the smallest one (both inclusive).

  * A label that precedes the table itself.

  * A label to jump to if the index has a value outside the bounds.

  The table is an ``addr_vec`` or ``addr_diff_vec`` inside of a
  ``jump_table_data``.  The number of elements in the table is one plus the
  difference between the upper bound and the lower bound.

  .. index:: tablejump instruction pattern

tablejump
  Instruction to jump to a variable address.  This is a low-level
  capability which can be used to implement a dispatch table when there
  is no :samp:`casesi` pattern.

  This pattern requires two operands: the address or offset, and a label
  which should immediately precede the jump table.  If the macro
  ``CASE_VECTOR_PC_RELATIVE`` evaluates to a nonzero value then the first
  operand is an offset which counts from the address of the table; otherwise,
  it is an absolute address to jump to.  In either case, the first operand has
  mode ``Pmode``.

  The :samp:`tablejump` insn is always the last insn before the jump
  table it uses.  Its assembler code normally has no need to use the
  second operand, but you should incorporate it in the RTL pattern so
  that the jump optimizer will not delete the table as unreachable code.

  .. index:: doloop_end instruction pattern

doloop_end
  Conditional branch instruction that decrements a register and
  jumps if the register is nonzero.  Operand 0 is the register to
  decrement and test; operand 1 is the label to jump to if the
  register is nonzero.
  See :ref:`looping-patterns`.

  This optional instruction pattern should be defined for machines with
  low-overhead looping instructions as the loop optimizer will try to
  modify suitable loops to utilize it.  The target hook
  ``TARGET_CAN_USE_DOLOOP_P`` controls the conditions under which
  low-overhead loops can be used.

  .. index:: doloop_begin instruction pattern

doloop_begin
  Companion instruction to ``doloop_end`` required for machines that
  need to perform some initialization, such as loading a special counter
  register.  Operand 1 is the associated ``doloop_end`` pattern and
  operand 0 is the register that it decrements.

  If initialization insns do not always need to be emitted, use a
  ``define_expand`` (see :ref:`expander-definitions`) and make it fail.

  .. index:: canonicalize_funcptr_for_compare instruction pattern

canonicalize_funcptr_for_compare
  Canonicalize the function pointer in operand 1 and store the result
  into operand 0.

  Operand 0 is always a ``reg`` and has mode ``Pmode`` ; operand 1
  may be a ``reg``, ``mem``, ``symbol_ref``, ``const_int``, etc
  and also has mode ``Pmode``.

  Canonicalization of a function pointer usually involves computing
  the address of the function which would be called if the function
  pointer were used in an indirect call.

  Only define this pattern if function pointers on the target machine
  can have different values but still call the same function when
  used in an indirect call.

  .. index:: save_stack_block instruction pattern, save_stack_function instruction pattern, save_stack_nonlocal instruction pattern, restore_stack_block instruction pattern, restore_stack_function instruction pattern, restore_stack_nonlocal instruction pattern

save_stack_block save_stack_function save_stack_nonlocal restore_stack_block restore_stack_function restore_stack_nonlocal
  Most machines save and restore the stack pointer by copying it to or
  from an object of mode ``Pmode``.  Do not define these patterns on
  such machines.

  Some machines require special handling for stack pointer saves and
  restores.  On those machines, define the patterns corresponding to the
  non-standard cases by using a ``define_expand`` (see :ref:`expander-definitions`) that produces the required insns.  The three types of
  saves and restores are:

  * :samp:`save_stack_block` saves the stack pointer at the start of a block
    that allocates a variable-sized object, and :samp:`restore_stack_block`
    restores the stack pointer when the block is exited.

  * :samp:`save_stack_function` and :samp:`restore_stack_function` do a
    similar job for the outermost block of a function and are used when the
    function allocates variable-sized objects or calls ``alloca``.  Only
    the epilogue uses the restored stack pointer, allowing a simpler save or
    restore sequence on some machines.

  * :samp:`save_stack_nonlocal` is used in functions that contain labels
    branched to by nested functions.  It saves the stack pointer in such a
    way that the inner function can use :samp:`restore_stack_nonlocal` to
    restore the stack pointer.  The compiler generates code to restore the
    frame and argument pointer registers, but some machines require saving
    and restoring additional data such as register window information or
    stack backchains.  Place insns in these patterns to save and restore any
    such required data.

  When saving the stack pointer, operand 0 is the save area and operand 1
  is the stack pointer.  The mode used to allocate the save area defaults
  to ``Pmode`` but you can override that choice by defining the
  ``STACK_SAVEAREA_MODE`` macro (see :ref:`storage-layout`).  You must
  specify an integral mode, or ``VOIDmode`` if no save area is needed
  for a particular type of save (either because no save is needed or
  because a machine-specific save area can be used).  Operand 0 is the
  stack pointer and operand 1 is the save area for restore operations.  If
  :samp:`save_stack_block` is defined, operand 0 must not be
  ``VOIDmode`` since these saves can be arbitrarily nested.

  A save area is a ``mem`` that is at a constant offset from
  ``virtual_stack_vars_rtx`` when the stack pointer is saved for use by
  nonlocal gotos and a ``reg`` in the other two cases.

  .. index:: allocate_stack instruction pattern

allocate_stack
  Subtract (or add if ``STACK_GROWS_DOWNWARD`` is undefined) operand 1 from
  the stack pointer to create space for dynamically allocated data.

  Store the resultant pointer to this space into operand 0.  If you
  are allocating space from the main stack, do this by emitting a
  move insn to copy ``virtual_stack_dynamic_rtx`` to operand 0.
  If you are allocating the space elsewhere, generate code to copy the
  location of the space to operand 0.  In the latter case, you must
  ensure this space gets freed when the corresponding space on the main
  stack is free.

  Do not define this pattern if all that must be done is the subtraction.
  Some machines require other operations such as stack probes or
  maintaining the back chain.  Define this pattern to emit those
  operations in addition to updating the stack pointer.

  .. index:: check_stack instruction pattern

check_stack
  If stack checking (see :ref:`stack-checking`) cannot be done on your system by
  probing the stack, define this pattern to perform the needed check and signal
  an error if the stack has overflowed.  The single operand is the address in
  the stack farthest from the current stack pointer that you need to validate.
  Normally, on platforms where this pattern is needed, you would obtain the
  stack limit from a global or thread-specific variable or register.

  .. index:: probe_stack_address instruction pattern

probe_stack_address
  If stack checking (see :ref:`stack-checking`) can be done on your system by
  probing the stack but without the need to actually access it, define this
  pattern and signal an error if the stack has overflowed.  The single operand
  is the memory address in the stack that needs to be probed.

  .. index:: probe_stack instruction pattern

probe_stack
  If stack checking (see :ref:`stack-checking`) can be done on your system by
  probing the stack but doing it with a 'store zero' instruction is not valid
  or optimal, define this pattern to do the probing differently and signal an
  error if the stack has overflowed.  The single operand is the memory reference
  in the stack that needs to be probed.

  .. index:: nonlocal_goto instruction pattern

nonlocal_goto
  Emit code to generate a non-local goto, e.g., a jump from one function
  to a label in an outer function.  This pattern has four arguments,
  each representing a value to be used in the jump.  The first
  argument is to be loaded into the frame pointer, the second is
  the address to branch to (code to dispatch to the actual label),
  the third is the address of a location where the stack is saved,
  and the last is the address of the label, to be placed in the
  location for the incoming static chain.

  On most machines you need not define this pattern, since GCC will
  already generate the correct code, which is to load the frame pointer
  and static chain, restore the stack (using the
  :samp:`restore_stack_nonlocal` pattern, if defined), and jump indirectly
  to the dispatcher.  You need only define this pattern if this code will
  not work on your machine.

  .. index:: nonlocal_goto_receiver instruction pattern

nonlocal_goto_receiver
  This pattern, if defined, contains code needed at the target of a
  nonlocal goto after the code already generated by GCC.  You will not
  normally need to define this pattern.  A typical reason why you might
  need this pattern is if some value, such as a pointer to a global table,
  must be restored when the frame pointer is restored.  Note that a nonlocal
  goto only occurs within a unit-of-translation, so a global table pointer
  that is shared by all functions of a given module need not be restored.
  There are no arguments.

  .. index:: exception_receiver instruction pattern

exception_receiver
  This pattern, if defined, contains code needed at the site of an
  exception handler that isn't needed at the site of a nonlocal goto.  You
  will not normally need to define this pattern.  A typical reason why you
  might need this pattern is if some value, such as a pointer to a global
  table, must be restored after control flow is branched to the handler of
  an exception.  There are no arguments.

  .. index:: builtin_setjmp_setup instruction pattern

builtin_setjmp_setup
  This pattern, if defined, contains additional code needed to initialize
  the ``jmp_buf``.  You will not normally need to define this pattern.
  A typical reason why you might need this pattern is if some value, such
  as a pointer to a global table, must be restored.  Though it is
  preferred that the pointer value be recalculated if possible (given the
  address of a label for instance).  The single argument is a pointer to
  the ``jmp_buf``.  Note that the buffer is five words long and that
  the first three are normally used by the generic mechanism.

  .. index:: builtin_setjmp_receiver instruction pattern

builtin_setjmp_receiver
  This pattern, if defined, contains code needed at the site of a
  built-in setjmp that isn't needed at the site of a nonlocal goto.  You
  will not normally need to define this pattern.  A typical reason why you
  might need this pattern is if some value, such as a pointer to a global
  table, must be restored.  It takes one argument, which is the label
  to which builtin_longjmp transferred control; this pattern may be emitted
  at a small offset from that label.

  .. index:: builtin_longjmp instruction pattern

builtin_longjmp
  This pattern, if defined, performs the entire action of the longjmp.
  You will not normally need to define this pattern unless you also define
  ``builtin_setjmp_setup``.  The single argument is a pointer to the
  ``jmp_buf``.

  .. index:: eh_return instruction pattern

eh_return
  This pattern, if defined, affects the way ``__builtin_eh_return``,
  and thence the call frame exception handling library routines, are
  built.  It is intended to handle non-trivial actions needed along
  the abnormal return path.

  The address of the exception handler to which the function should return
  is passed as operand to this pattern.  It will normally need to copied by
  the pattern to some special register or memory location.
  If the pattern needs to determine the location of the target call
  frame in order to do so, it may use ``EH_RETURN_STACKADJ_RTX``,
  if defined; it will have already been assigned.

  If this pattern is not defined, the default action will be to simply
  copy the return address to ``EH_RETURN_HANDLER_RTX``.  Either
  that macro or this pattern needs to be defined if call frame exception
  handling is to be used.

  .. index:: prologue instruction pattern

.. _prologue-instruction-pattern:

prologue
  This pattern, if defined, emits RTL for entry to a function.  The function
  entry is responsible for setting up the stack frame, initializing the frame
  pointer register, saving callee saved registers, etc.

  Using a prologue pattern is generally preferred over defining
  ``TARGET_ASM_FUNCTION_PROLOGUE`` to emit assembly code for the prologue.

  The ``prologue`` pattern is particularly useful for targets which perform
  instruction scheduling.

  .. index:: window_save instruction pattern

.. _window_save-instruction-pattern:

window_save
  This pattern, if defined, emits RTL for a register window save.  It should
  be defined if the target machine has register windows but the window events
  are decoupled from calls to subroutines.  The canonical example is the SPARC
  architecture.

  .. index:: epilogue instruction pattern

.. _epilogue-instruction-pattern:

epilogue
  This pattern emits RTL for exit from a function.  The function
  exit is responsible for deallocating the stack frame, restoring callee saved
  registers and emitting the return instruction.

  Using an epilogue pattern is generally preferred over defining
  ``TARGET_ASM_FUNCTION_EPILOGUE`` to emit assembly code for the epilogue.

  The ``epilogue`` pattern is particularly useful for targets which perform
  instruction scheduling or which have delay slots for their return instruction.

  .. index:: sibcall_epilogue instruction pattern

sibcall_epilogue
  This pattern, if defined, emits RTL for exit from a function without the final
  branch back to the calling function.  This pattern will be emitted before any
  sibling call (aka tail call) sites.

  The ``sibcall_epilogue`` pattern must not clobber any arguments used for
  parameter passing or any stack slots for arguments passed to the current
  function.

  .. index:: trap instruction pattern

trap
  This pattern, if defined, signals an error, typically by causing some
  kind of signal to be raised.

  .. index:: ctrapMM4 instruction pattern

ctrapMM4
  Conditional trap instruction.  Operand 0 is a piece of RTL which
  performs a comparison, and operands 1 and 2 are the arms of the
  comparison.  Operand 3 is the trap code, an integer.

  A typical ``ctrap`` pattern looks like

  .. code-block::

    (define_insn "ctrapsi4"
      [(trap_if (match_operator 0 "trap_operator"
                 [(match_operand 1 "register_operand")
                  (match_operand 2 "immediate_operand")])
                (match_operand 3 "const_int_operand" "i"))]
      ""
      "...")

  .. index:: prefetch instruction pattern

prefetch
  This pattern, if defined, emits code for a non-faulting data prefetch
  instruction.  Operand 0 is the address of the memory to prefetch.  Operand 1
  is a constant 1 if the prefetch is preparing for a write to the memory
  address, or a constant 0 otherwise.  Operand 2 is the expected degree of
  temporal locality of the data and is a value between 0 and 3, inclusive; 0
  means that the data has no temporal locality, so it need not be left in the
  cache after the access; 3 means that the data has a high degree of temporal
  locality and should be left in all levels of cache possible;  1 and 2 mean,
  respectively, a low or moderate degree of temporal locality.

  Targets that do not support write prefetches or locality hints can ignore
  the values of operands 1 and 2.

  .. index:: blockage instruction pattern

blockage
  This pattern defines a pseudo insn that prevents the instruction
  scheduler and other passes from moving instructions and using register
  equivalences across the boundary defined by the blockage insn.
  This needs to be an UNSPEC_VOLATILE pattern or a volatile ASM.

  .. index:: memory_blockage instruction pattern

memory_blockage
  This pattern, if defined, represents a compiler memory barrier, and will be
  placed at points across which RTL passes may not propagate memory accesses.
  This instruction needs to read and write volatile BLKmode memory.  It does
  not need to generate any machine instruction.  If this pattern is not defined,
  the compiler falls back to emitting an instruction corresponding
  to ``asm volatile ("" ::: "memory")``.

  .. index:: memory_barrier instruction pattern

memory_barrier
  If the target memory model is not fully synchronous, then this pattern
  should be defined to an instruction that orders both loads and stores
  before the instruction with respect to loads and stores after the instruction.
  This pattern has no operands.

  .. index:: speculation_barrier instruction pattern

speculation_barrier
  If the target can support speculative execution, then this pattern should
  be defined to an instruction that will block subsequent execution until
  any prior speculation conditions has been resolved.  The pattern must also
  ensure that the compiler cannot move memory operations past the barrier,
  so it needs to be an UNSPEC_VOLATILE pattern.  The pattern has no
  operands.

  If this pattern is not defined then the default expansion of
  ``__builtin_speculation_safe_value`` will emit a warning.  You can
  suppress this warning by defining this pattern with a final condition
  of ``0`` (zero), which tells the compiler that a speculation
  barrier is not needed for this target.

  .. index:: sync_compare_and_swapmode instruction pattern

sync_compare_and_swapmode
  This pattern, if defined, emits code for an atomic compare-and-swap
  operation.  Operand 1 is the memory on which the atomic operation is
  performed.  Operand 2 is the 'old' value to be compared against the
  current contents of the memory location.  Operand 3 is the 'new' value
  to store in the memory if the compare succeeds.  Operand 0 is the result
  of the operation; it should contain the contents of the memory
  before the operation.  If the compare succeeds, this should obviously be
  a copy of operand 2.

  This pattern must show that both operand 0 and operand 1 are modified.

  This pattern must issue any memory barrier instructions such that all
  memory operations before the atomic operation occur before the atomic
  operation and all memory operations after the atomic operation occur
  after the atomic operation.

  For targets where the success or failure of the compare-and-swap
  operation is available via the status flags, it is possible to
  avoid a separate compare operation and issue the subsequent
  branch or store-flag operation immediately after the compare-and-swap.
  To this end, GCC will look for a ``MODE_CC`` set in the
  output of ``sync_compare_and_swapmode`` ; if the machine
  description includes such a set, the target should also define special
  ``cbranchcc4`` and/or ``cstorecc4`` instructions.  GCC will then
  be able to take the destination of the ``MODE_CC`` set and pass it
  to the ``cbranchcc4`` or ``cstorecc4`` pattern as the first
  operand of the comparison (the second will be ``(const_int 0)``).

  For targets where the operating system may provide support for this
  operation via library calls, the ``sync_compare_and_swap_optab``
  may be initialized to a function with the same interface as the
  ``__sync_val_compare_and_swap_n`` built-in.  If the entire
  set of :samp:`{__sync}` builtins are supported via library calls, the
  target can initialize all of the optabs at once with
  ``init_sync_libfuncs``.
  For the purposes of C++11 ``std::atomic::is_lock_free``, it is
  assumed that these library calls do *not* use any kind of
  interruptable locking.

  .. index:: sync_addmode instruction pattern, sync_submode instruction pattern, sync_iormode instruction pattern, sync_andmode instruction pattern, sync_xormode instruction pattern, sync_nandmode instruction pattern

:samp:`sync_add{mode}`, :samp:`sync_sub{mode}` :samp:`sync_ior{mode}`, :samp:`sync_and{mode}` :samp:`sync_xor{mode}`, :samp:`sync_nand{mode}`
  These patterns emit code for an atomic operation on memory.
  Operand 0 is the memory on which the atomic operation is performed.
  Operand 1 is the second operand to the binary operator.

  This pattern must issue any memory barrier instructions such that all
  memory operations before the atomic operation occur before the atomic
  operation and all memory operations after the atomic operation occur
  after the atomic operation.

  If these patterns are not defined, the operation will be constructed
  from a compare-and-swap operation, if defined.

  .. index:: sync_old_addmode instruction pattern, sync_old_submode instruction pattern, sync_old_iormode instruction pattern, sync_old_andmode instruction pattern, sync_old_xormode instruction pattern, sync_old_nandmode instruction pattern

:samp:`sync_old_add{mode}`, :samp:`sync_old_sub{mode}` :samp:`sync_old_ior{mode}`, :samp:`sync_old_and{mode}` :samp:`sync_old_xor{mode}`, :samp:`sync_old_nand{mode}`
  These patterns emit code for an atomic operation on memory,
  and return the value that the memory contained before the operation.
  Operand 0 is the result value, operand 1 is the memory on which the
  atomic operation is performed, and operand 2 is the second operand
  to the binary operator.

  This pattern must issue any memory barrier instructions such that all
  memory operations before the atomic operation occur before the atomic
  operation and all memory operations after the atomic operation occur
  after the atomic operation.

  If these patterns are not defined, the operation will be constructed
  from a compare-and-swap operation, if defined.

  .. index:: sync_new_addmode instruction pattern, sync_new_submode instruction pattern, sync_new_iormode instruction pattern, sync_new_andmode instruction pattern, sync_new_xormode instruction pattern, sync_new_nandmode instruction pattern

:samp:`sync_new_add{mode}`, :samp:`sync_new_sub{mode}` :samp:`sync_new_ior{mode}`, :samp:`sync_new_and{mode}` :samp:`sync_new_xor{mode}`, :samp:`sync_new_nand{mode}`
  These patterns are like their ``sync_old_op`` counterparts,
  except that they return the value that exists in the memory location
  after the operation, rather than before the operation.

  .. index:: sync_lock_test_and_setmode instruction pattern

sync_lock_test_and_setmode
  This pattern takes two forms, based on the capabilities of the target.
  In either case, operand 0 is the result of the operand, operand 1 is
  the memory on which the atomic operation is performed, and operand 2
  is the value to set in the lock.

  In the ideal case, this operation is an atomic exchange operation, in
  which the previous value in memory operand is copied into the result
  operand, and the value operand is stored in the memory operand.

  For less capable targets, any value operand that is not the constant 1
  should be rejected with ``FAIL``.  In this case the target may use
  an atomic test-and-set bit operation.  The result operand should contain
  1 if the bit was previously set and 0 if the bit was previously clear.
  The true contents of the memory operand are implementation defined.

  This pattern must issue any memory barrier instructions such that the
  pattern as a whole acts as an acquire barrier, that is all memory
  operations after the pattern do not occur until the lock is acquired.

  If this pattern is not defined, the operation will be constructed from
  a compare-and-swap operation, if defined.

  .. index:: sync_lock_releasemode instruction pattern

sync_lock_releasemode
  This pattern, if defined, releases a lock set by
  ``sync_lock_test_and_setmode``.  Operand 0 is the memory
  that contains the lock; operand 1 is the value to store in the lock.

  If the target doesn't implement full semantics for
  ``sync_lock_test_and_setmode``, any value operand which is not
  the constant 0 should be rejected with ``FAIL``, and the true contents
  of the memory operand are implementation defined.

  This pattern must issue any memory barrier instructions such that the
  pattern as a whole acts as a release barrier, that is the lock is
  released only after all previous memory operations have completed.

  If this pattern is not defined, then a ``memory_barrier`` pattern
  will be emitted, followed by a store of the value to the memory operand.

  .. index:: atomic_compare_and_swapmode instruction pattern

atomic_compare_and_swapmode
  This pattern, if defined, emits code for an atomic compare-and-swap
  operation with memory model semantics.  Operand 2 is the memory on which
  the atomic operation is performed.  Operand 0 is an output operand which
  is set to true or false based on whether the operation succeeded.  Operand
  1 is an output operand which is set to the contents of the memory before
  the operation was attempted.  Operand 3 is the value that is expected to
  be in memory.  Operand 4 is the value to put in memory if the expected
  value is found there.  Operand 5 is set to 1 if this compare and swap is to
  be treated as a weak operation.  Operand 6 is the memory model to be used
  if the operation is a success.  Operand 7 is the memory model to be used
  if the operation fails.

  If memory referred to in operand 2 contains the value in operand 3, then
  operand 4 is stored in memory pointed to by operand 2 and fencing based on
  the memory model in operand 6 is issued.

  If memory referred to in operand 2 does not contain the value in operand 3,
  then fencing based on the memory model in operand 7 is issued.

  If a target does not support weak compare-and-swap operations, or the port
  elects not to implement weak operations, the argument in operand 5 can be
  ignored.  Note a strong implementation must be provided.

  If this pattern is not provided, the ``__atomic_compare_exchange``
  built-in functions will utilize the legacy ``sync_compare_and_swap``
  pattern with an ``__ATOMIC_SEQ_CST`` memory model.

  .. index:: atomic_loadmode instruction pattern

atomic_loadmode
  This pattern implements an atomic load operation with memory model
  semantics.  Operand 1 is the memory address being loaded from.  Operand 0
  is the result of the load.  Operand 2 is the memory model to be used for
  the load operation.

  If not present, the ``__atomic_load`` built-in function will either
  resort to a normal load with memory barriers, or a compare-and-swap
  operation if a normal load would not be atomic.

  .. index:: atomic_storemode instruction pattern

atomic_storemode
  This pattern implements an atomic store operation with memory model
  semantics.  Operand 0 is the memory address being stored to.  Operand 1
  is the value to be written.  Operand 2 is the memory model to be used for
  the operation.

  If not present, the ``__atomic_store`` built-in function will attempt to
  perform a normal store and surround it with any required memory fences.  If
  the store would not be atomic, then an ``__atomic_exchange`` is
  attempted with the result being ignored.

  .. index:: atomic_exchangemode instruction pattern

atomic_exchangemode
  This pattern implements an atomic exchange operation with memory model
  semantics.  Operand 1 is the memory location the operation is performed on.
  Operand 0 is an output operand which is set to the original value contained
  in the memory pointed to by operand 1.  Operand 2 is the value to be
  stored.  Operand 3 is the memory model to be used.

  If this pattern is not present, the built-in function
  ``__atomic_exchange`` will attempt to preform the operation with a
  compare and swap loop.

  .. index:: atomic_addmode instruction pattern, atomic_submode instruction pattern, atomic_ormode instruction pattern, atomic_andmode instruction pattern, atomic_xormode instruction pattern, atomic_nandmode instruction pattern

:samp:`atomic_add{mode}`, :samp:`atomic_sub{mode}` :samp:`atomic_or{mode}`, :samp:`atomic_and{mode}` :samp:`atomic_xor{mode}`, :samp:`atomic_nand{mode}`
  These patterns emit code for an atomic operation on memory with memory
  model semantics. Operand 0 is the memory on which the atomic operation is
  performed.  Operand 1 is the second operand to the binary operator.
  Operand 2 is the memory model to be used by the operation.

  If these patterns are not defined, attempts will be made to use legacy
  ``sync`` patterns, or equivalent patterns which return a result.  If
  none of these are available a compare-and-swap loop will be used.

  .. index:: atomic_fetch_addmode instruction pattern, atomic_fetch_submode instruction pattern, atomic_fetch_ormode instruction pattern, atomic_fetch_andmode instruction pattern, atomic_fetch_xormode instruction pattern, atomic_fetch_nandmode instruction pattern

:samp:`atomic_fetch_add{mode}`, :samp:`atomic_fetch_sub{mode}` :samp:`atomic_fetch_or{mode}`, :samp:`atomic_fetch_and{mode}` :samp:`atomic_fetch_xor{mode}`, :samp:`atomic_fetch_nand{mode}`
  These patterns emit code for an atomic operation on memory with memory
  model semantics, and return the original value. Operand 0 is an output
  operand which contains the value of the memory location before the
  operation was performed.  Operand 1 is the memory on which the atomic
  operation is performed.  Operand 2 is the second operand to the binary
  operator.  Operand 3 is the memory model to be used by the operation.

  If these patterns are not defined, attempts will be made to use legacy
  ``sync`` patterns.  If none of these are available a compare-and-swap
  loop will be used.

  .. index:: atomic_add_fetchmode instruction pattern, atomic_sub_fetchmode instruction pattern, atomic_or_fetchmode instruction pattern, atomic_and_fetchmode instruction pattern, atomic_xor_fetchmode instruction pattern, atomic_nand_fetchmode instruction pattern

:samp:`atomic_add_fetch{mode}`, :samp:`atomic_sub_fetch{mode}` :samp:`atomic_or_fetch{mode}`, :samp:`atomic_and_fetch{mode}` :samp:`atomic_xor_fetch{mode}`, :samp:`atomic_nand_fetch{mode}`
  These patterns emit code for an atomic operation on memory with memory
  model semantics and return the result after the operation is performed.
  Operand 0 is an output operand which contains the value after the
  operation.  Operand 1 is the memory on which the atomic operation is
  performed.  Operand 2 is the second operand to the binary operator.
  Operand 3 is the memory model to be used by the operation.

  If these patterns are not defined, attempts will be made to use legacy
  ``sync`` patterns, or equivalent patterns which return the result before
  the operation followed by the arithmetic operation required to produce the
  result.  If none of these are available a compare-and-swap loop will be
  used.

  .. index:: atomic_test_and_set instruction pattern

atomic_test_and_set
  This pattern emits code for ``__builtin_atomic_test_and_set``.
  Operand 0 is an output operand which is set to true if the previous
  previous contents of the byte was "set", and false otherwise.  Operand 1
  is the ``QImode`` memory to be modified.  Operand 2 is the memory
  model to be used.

  The specific value that defines "set" is implementation defined, and
  is normally based on what is performed by the native atomic test and set
  instruction.

  .. index:: atomic_bit_test_and_setmode instruction pattern, atomic_bit_test_and_complementmode instruction pattern, atomic_bit_test_and_resetmode instruction pattern

atomic_bit_test_and_setmode atomic_bit_test_and_complementmode atomic_bit_test_and_resetmode
  These patterns emit code for an atomic bitwise operation on memory with memory
  model semantics, and return the original value of the specified bit.
  Operand 0 is an output operand which contains the value of the specified bit
  from the memory location before the operation was performed.  Operand 1 is the
  memory on which the atomic operation is performed.  Operand 2 is the bit within
  the operand, starting with least significant bit.  Operand 3 is the memory model
  to be used by the operation.  Operand 4 is a flag - it is ``const1_rtx``
  if operand 0 should contain the original value of the specified bit in the
  least significant bit of the operand, and ``const0_rtx`` if the bit should
  be in its original position in the operand.
  ``atomic_bit_test_and_setmode`` atomically sets the specified bit after
  remembering its original value, ``atomic_bit_test_and_complementmode``
  inverts the specified bit and ``atomic_bit_test_and_resetmode`` clears
  the specified bit.

  If these patterns are not defined, attempts will be made to use
  ``atomic_fetch_ormode``, ``atomic_fetch_xormode`` or
  ``atomic_fetch_andmode`` instruction patterns, or their ``sync``
  counterparts.  If none of these are available a compare-and-swap
  loop will be used.

  .. index:: atomic_add_fetch_cmp_0mode instruction pattern, atomic_sub_fetch_cmp_0mode instruction pattern, atomic_and_fetch_cmp_0mode instruction pattern, atomic_or_fetch_cmp_0mode instruction pattern, atomic_xor_fetch_cmp_0mode instruction pattern

atomic_add_fetch_cmp_0mode atomic_sub_fetch_cmp_0mode atomic_and_fetch_cmp_0mode atomic_or_fetch_cmp_0mode atomic_xor_fetch_cmp_0mode
  These patterns emit code for an atomic operation on memory with memory
  model semantics if the fetch result is used only in a comparison against
  zero.
  Operand 0 is an output operand which contains a boolean result of comparison
  of the value after the operation against zero.  Operand 1 is the memory on
  which the atomic operation is performed.  Operand 2 is the second operand
  to the binary operator.  Operand 3 is the memory model to be used by the
  operation.  Operand 4 is an integer holding the comparison code, one of
  ``EQ``, ``NE``, ``LT``, ``GT``, ``LE`` or ``GE``.

  If these patterns are not defined, attempts will be made to use separate
  atomic operation and fetch pattern followed by comparison of the result
  against zero.

  .. index:: mem_thread_fence instruction pattern

mem_thread_fence
  This pattern emits code required to implement a thread fence with
  memory model semantics.  Operand 0 is the memory model to be used.

  For the ``__ATOMIC_RELAXED`` model no instructions need to be issued
  and this expansion is not invoked.

  The compiler always emits a compiler memory barrier regardless of what
  expanding this pattern produced.

  If this pattern is not defined, the compiler falls back to expanding the
  ``memory_barrier`` pattern, then to emitting ``__sync_synchronize``
  library call, and finally to just placing a compiler memory barrier.

  .. index:: get_thread_pointermode instruction pattern, set_thread_pointermode instruction pattern

get_thread_pointermode set_thread_pointermode
  These patterns emit code that reads/sets the TLS thread pointer. Currently,
  these are only needed if the target needs to support the
  ``__builtin_thread_pointer`` and ``__builtin_set_thread_pointer``
  builtins.

  The get/set patterns have a single output/input operand respectively,
  with :samp:`{mode}` intended to be ``Pmode``.

  .. index:: stack_protect_combined_set instruction pattern

stack_protect_combined_set
  This pattern, if defined, moves a ``ptr_mode`` value from an address
  whose declaration RTX is given in operand 1 to the memory in operand 0
  without leaving the value in a register afterward.  If several
  instructions are needed by the target to perform the operation (eg. to
  load the address from a GOT entry then load the ``ptr_mode`` value
  and finally store it), it is the backend's responsibility to ensure no
  intermediate result gets spilled.  This is to avoid leaking the value
  some place that an attacker might use to rewrite the stack guard slot
  after having clobbered it.

  If this pattern is not defined, then the address declaration is
  expanded first in the standard way and a ``stack_protect_set``
  pattern is then generated to move the value from that address to the
  address in operand 0.

  .. index:: stack_protect_set instruction pattern

stack_protect_set
  This pattern, if defined, moves a ``ptr_mode`` value from the valid
  memory location in operand 1 to the memory in operand 0 without leaving
  the value in a register afterward.  This is to avoid leaking the value
  some place that an attacker might use to rewrite the stack guard slot
  after having clobbered it.

  Note: on targets where the addressing modes do not allow to load
  directly from stack guard address, the address is expanded in a standard
  way first which could cause some spills.

  If this pattern is not defined, then a plain move pattern is generated.

  .. index:: stack_protect_combined_test instruction pattern

stack_protect_combined_test
  This pattern, if defined, compares a ``ptr_mode`` value from an
  address whose declaration RTX is given in operand 1 with the memory in
  operand 0 without leaving the value in a register afterward and
  branches to operand 2 if the values were equal.  If several
  instructions are needed by the target to perform the operation (eg. to
  load the address from a GOT entry then load the ``ptr_mode`` value
  and finally store it), it is the backend's responsibility to ensure no
  intermediate result gets spilled.  This is to avoid leaking the value
  some place that an attacker might use to rewrite the stack guard slot
  after having clobbered it.

  If this pattern is not defined, then the address declaration is
  expanded first in the standard way and a ``stack_protect_test``
  pattern is then generated to compare the value from that address to the
  value at the memory in operand 0.

  .. index:: stack_protect_test instruction pattern

stack_protect_test
  This pattern, if defined, compares a ``ptr_mode`` value from the
  valid memory location in operand 1 with the memory in operand 0 without
  leaving the value in a register afterward and branches to operand 2 if
  the values were equal.

  If this pattern is not defined, then a plain compare pattern and
  conditional branch pattern is used.

  .. index:: clear_cache instruction pattern

clear_cache
  This pattern, if defined, flushes the instruction cache for a region of
  memory.  The region is bounded to by the Pmode pointers in operand 0
  inclusive and operand 1 exclusive.

  If this pattern is not defined, a call to the library function
  ``__clear_cache`` is used.

  .. index:: spaceshipm3 instruction pattern

spaceshipm3
  Initialize output operand 0 with mode of integer type to -1, 0, 1 or 2
  if operand 1 with mode :samp:`{m}` compares less than operand 2, equal to
  operand 2, greater than operand 2 or is unordered with operand 2.
  :samp:`{m}` should be a scalar floating point mode.

  This pattern is not allowed to ``FAIL``.