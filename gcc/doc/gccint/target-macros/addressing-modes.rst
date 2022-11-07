..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: addressing modes

.. _addressing-modes:

Addressing Modes
****************

.. prevent bad page break with this line

This is about addressing modes.

.. c:macro:: HAVE_PRE_INCREMENT
             HAVE_PRE_DECREMENT
             HAVE_POST_INCREMENT
             HAVE_POST_DECREMENT

  A C expression that is nonzero if the machine supports pre-increment,
  pre-decrement, post-increment, or post-decrement addressing respectively.

.. c:macro:: HAVE_PRE_MODIFY_DISP
             HAVE_POST_MODIFY_DISP

  A C expression that is nonzero if the machine supports pre- or
  post-address side-effect generation involving constants other than
  the size of the memory operand.

.. c:macro:: HAVE_PRE_MODIFY_REG
             HAVE_POST_MODIFY_REG

  A C expression that is nonzero if the machine supports pre- or
  post-address side-effect generation involving a register displacement.

.. c:macro:: CONSTANT_ADDRESS_P (x)

  A C expression that is 1 if the RTX :samp:`{x}` is a constant which
  is a valid address.  On most machines the default definition of
  ``(CONSTANT_P (x) && GET_CODE (x) != CONST_DOUBLE)``
  is acceptable, but a few machines are more restrictive as to which
  constant addresses are supported.

.. c:macro:: CONSTANT_P (x)

  ``CONSTANT_P``, which is defined by target-independent code,
  accepts integer-values expressions whose values are not explicitly
  known, such as ``symbol_ref``, ``label_ref``, and ``high``
  expressions and ``const`` arithmetic expressions, in addition to
  ``const_int`` and ``const_double`` expressions.

.. c:macro:: MAX_REGS_PER_ADDRESS

  A number, the maximum number of registers that can appear in a valid
  memory address.  Note that it is up to you to specify a value equal to
  the maximum number that ``TARGET_LEGITIMATE_ADDRESS_P`` would ever
  accept.

.. function:: bool TARGET_LEGITIMATE_ADDRESS_P (machine_mode mode, rtx x, bool strict)

  .. hook-start:TARGET_LEGITIMATE_ADDRESS_P

  A function that returns whether :samp:`{x}` (an RTX) is a legitimate memory
  address on the target machine for a memory operand of mode :samp:`{mode}`.

  Legitimate addresses are defined in two variants: a strict variant and a
  non-strict one.  The :samp:`{strict}` parameter chooses which variant is
  desired by the caller.

  The strict variant is used in the reload pass.  It must be defined so
  that any pseudo-register that has not been allocated a hard register is
  considered a memory reference.  This is because in contexts where some
  kind of register is required, a pseudo-register with no hard register
  must be rejected.  For non-hard registers, the strict variant should look
  up the ``reg_renumber`` array; it should then proceed using the hard
  register number in the array, or treat the pseudo as a memory reference
  if the array holds ``-1``.

  The non-strict variant is used in other passes.  It must be defined to
  accept all pseudo-registers in every context where some kind of
  register is required.

  Normally, constant addresses which are the sum of a ``symbol_ref``
  and an integer are stored inside a ``const`` RTX to mark them as
  constant.  Therefore, there is no need to recognize such sums
  specifically as legitimate addresses.  Normally you would simply
  recognize any ``const`` as legitimate.

  Usually ``PRINT_OPERAND_ADDRESS`` is not prepared to handle constant
  sums that are not marked with  ``const``.  It assumes that a naked
  ``plus`` indicates indexing.  If so, then you *must* reject such
  naked constant sums as illegitimate addresses, so that none of them will
  be given to ``PRINT_OPERAND_ADDRESS``.

  .. index:: TARGET_ENCODE_SECTION_INFO and address validation

  On some machines, whether a symbolic address is legitimate depends on
  the section that the address refers to.  On these machines, define the
  target hook ``TARGET_ENCODE_SECTION_INFO`` to store the information
  into the ``symbol_ref``, and then check for it here.  When you see a
  ``const``, you will have to look inside it to find the
  ``symbol_ref`` in order to determine the section.  See :ref:`assembler-format`.

  .. index:: GO_IF_LEGITIMATE_ADDRESS

  Some ports are still using a deprecated legacy substitute for
  this hook, the ``GO_IF_LEGITIMATE_ADDRESS`` macro.  This macro
  has this syntax:

  .. code-block:: c++

    #define GO_IF_LEGITIMATE_ADDRESS (mode, x, label)

  and should ``goto label`` if the address :samp:`{x}` is a valid
  address on the target machine for a memory operand of mode :samp:`{mode}`.

  .. index:: REG_OK_STRICT

  Compiler source files that want to use the strict variant of this
  macro define the macro ``REG_OK_STRICT``.  You should use an
  ``#ifdef REG_OK_STRICT`` conditional to define the strict variant in
  that case and the non-strict variant otherwise.

  Using the hook is usually simpler because it limits the number of
  files that are recompiled when changes are made.

.. hook-end

.. c:macro:: TARGET_MEM_CONSTRAINT

  A single character to be used instead of the default ``'m'``
  character for general memory addresses.  This defines the constraint
  letter which matches the memory addresses accepted by
  ``TARGET_LEGITIMATE_ADDRESS_P``.  Define this macro if you want to
  support new address formats in your back end without changing the
  semantics of the ``'m'`` constraint.  This is necessary in order to
  preserve functionality of inline assembly constructs using the
  ``'m'`` constraint.

.. c:macro:: FIND_BASE_TERM (x)

  A C expression to determine the base term of address :samp:`{x}`,
  or to provide a simplified version of :samp:`{x}` from which :samp:`alias.cc`
  can easily find the base term.  This macro is used in only two places:
  ``find_base_value`` and ``find_base_term`` in :samp:`alias.cc`.

  It is always safe for this macro to not be defined.  It exists so
  that alias analysis can understand machine-dependent addresses.

  The typical use of this macro is to handle addresses containing
  a label_ref or symbol_ref within an UNSPEC.

.. function:: rtx TARGET_LEGITIMIZE_ADDRESS (rtx x, rtx oldx, machine_mode mode)

  .. hook-start:TARGET_LEGITIMIZE_ADDRESS

  This hook is given an invalid memory address :samp:`{x}` for an
  operand of mode :samp:`{mode}` and should try to return a valid memory
  address.

  .. index:: break_out_memory_refs

  :samp:`{x}` will always be the result of a call to ``break_out_memory_refs``,
  and :samp:`{oldx}` will be the operand that was given to that function to produce
  :samp:`{x}`.

  The code of the hook should not alter the substructure of
  :samp:`{x}`.  If it transforms :samp:`{x}` into a more legitimate form, it
  should return the new :samp:`{x}`.

  It is not necessary for this hook to come up with a legitimate address,
  with the exception of native TLS addresses (see :ref:`emulated-tls`).
  The compiler has standard ways of doing so in all cases.  In fact, if
  the target supports only emulated TLS, it
  is safe to omit this hook or make it return :samp:`{x}` if it cannot find
  a valid way to legitimize the address.  But often a machine-dependent
  strategy can generate better code.

.. hook-end

.. c:macro:: LEGITIMIZE_RELOAD_ADDRESS (x, mode, opnum, type, ind_levels, win)

  A C compound statement that attempts to replace :samp:`{x}`, which is an address
  that needs reloading, with a valid memory address for an operand of mode
  :samp:`{mode}`.  :samp:`{win}` will be a C statement label elsewhere in the code.
  It is not necessary to define this macro, but it might be useful for
  performance reasons.

  For example, on the i386, it is sometimes possible to use a single
  reload register instead of two by reloading a sum of two pseudo
  registers into a register.  On the other hand, for number of RISC
  processors offsets are limited so that often an intermediate address
  needs to be generated in order to address a stack slot.  By defining
  ``LEGITIMIZE_RELOAD_ADDRESS`` appropriately, the intermediate addresses
  generated for adjacent some stack slots can be made identical, and thus
  be shared.

  .. note::

    This macro should be used with caution.  It is necessary
    to know something of how reload works in order to effectively use this,
    and it is quite easy to produce macros that build in too much knowledge
    of reload internals.

  .. note::

    This macro must be able to reload an address created by a
    previous invocation of this macro.  If it fails to handle such addresses
    then the compiler may generate incorrect code or abort.

  .. index:: push_reload

  The macro definition should use ``push_reload`` to indicate parts that
  need reloading; :samp:`{opnum}`, :samp:`{type}` and :samp:`{ind_levels}` are usually
  suitable to be passed unaltered to ``push_reload``.

  The code generated by this macro must not alter the substructure of
  :samp:`{x}`.  If it transforms :samp:`{x}` into a more legitimate form, it
  should assign :samp:`{x}` (which will always be a C variable) a new value.
  This also applies to parts that you change indirectly by calling
  ``push_reload``.

  .. index:: strict_memory_address_p

  The macro definition may use ``strict_memory_address_p`` to test if
  the address has become legitimate.

  .. index:: copy_rtx

  If you want to change only a part of :samp:`{x}`, one standard way of doing
  this is to use ``copy_rtx``.  Note, however, that it unshares only a
  single level of rtl.  Thus, if the part to be changed is not at the
  top level, you'll need to replace first the top level.
  It is not necessary for this macro to come up with a legitimate
  address;  but often a machine-dependent strategy can generate better code.

.. function:: bool TARGET_MODE_DEPENDENT_ADDRESS_P (const_rtx addr, addr_space_t addrspace)

  .. hook-start:TARGET_MODE_DEPENDENT_ADDRESS_P

  This hook returns ``true`` if memory address :samp:`{addr}` in address
  space :samp:`{addrspace}` can have
  different meanings depending on the machine mode of the memory
  reference it is used for or if the address is valid for some modes
  but not others.

  Autoincrement and autodecrement addresses typically have mode-dependent
  effects because the amount of the increment or decrement is the size
  of the operand being addressed.  Some machines have other mode-dependent
  addresses.  Many RISC machines have no mode-dependent addresses.

  You may assume that :samp:`{addr}` is a valid address for the machine.

  The default version of this hook returns ``false``.

.. hook-end

.. function:: bool TARGET_LEGITIMATE_CONSTANT_P (machine_mode mode, rtx x)

  .. hook-start:TARGET_LEGITIMATE_CONSTANT_P

  This hook returns true if :samp:`{x}` is a legitimate constant for a
  :samp:`{mode}` -mode immediate operand on the target machine.  You can assume that
  :samp:`{x}` satisfies ``CONSTANT_P``, so you need not check this.

  The default definition returns true.

.. hook-end

.. function:: bool TARGET_PRECOMPUTE_TLS_P (machine_mode mode, rtx x)

  .. hook-start:TARGET_PRECOMPUTE_TLS_P

  This hook returns true if :samp:`{x}` is a TLS operand on the target
  machine that should be pre-computed when used as the argument in a call.
  You can assume that :samp:`{x}` satisfies ``CONSTANT_P``, so you need not
  check this.

  The default definition returns false.

.. hook-end

.. function:: rtx TARGET_DELEGITIMIZE_ADDRESS (rtx x)

  .. hook-start:TARGET_DELEGITIMIZE_ADDRESS

  This hook is used to undo the possibly obfuscating effects of the
  ``LEGITIMIZE_ADDRESS`` and ``LEGITIMIZE_RELOAD_ADDRESS`` target
  macros.  Some backend implementations of these macros wrap symbol
  references inside an ``UNSPEC`` rtx to represent PIC or similar
  addressing modes.  This target hook allows GCC's optimizers to understand
  the semantics of these opaque ``UNSPEC`` s by converting them back
  into their original form.

.. hook-end

.. function:: bool TARGET_CONST_NOT_OK_FOR_DEBUG_P (rtx x)

  .. hook-start:TARGET_CONST_NOT_OK_FOR_DEBUG_P

  This hook should return true if :samp:`{x}` should not be emitted into
  debug sections.

.. hook-end

.. function:: bool TARGET_CANNOT_FORCE_CONST_MEM (machine_mode mode, rtx x)

  .. hook-start:TARGET_CANNOT_FORCE_CONST_MEM

  This hook should return true if :samp:`{x}` is of a form that cannot (or
  should not) be spilled to the constant pool.  :samp:`{mode}` is the mode
  of :samp:`{x}`.

  The default version of this hook returns false.

  The primary reason to define this hook is to prevent reload from
  deciding that a non-legitimate constant would be better reloaded
  from the constant pool instead of spilling and reloading a register
  holding the constant.  This restriction is often true of addresses
  of TLS symbols for various targets.

.. hook-end

.. function:: bool TARGET_USE_BLOCKS_FOR_CONSTANT_P (machine_mode mode, const_rtx x)

  .. hook-start:TARGET_USE_BLOCKS_FOR_CONSTANT_P

  This hook should return true if pool entries for constant :samp:`{x}` can
  be placed in an ``object_block`` structure.  :samp:`{mode}` is the mode
  of :samp:`{x}`.

  The default version returns false for all constants.

.. hook-end

.. function:: bool TARGET_USE_BLOCKS_FOR_DECL_P (const_tree decl)

  .. hook-start:TARGET_USE_BLOCKS_FOR_DECL_P

  This hook should return true if pool entries for :samp:`{decl}` should
  be placed in an ``object_block`` structure.

  The default version returns true for all decls.

.. hook-end

.. function:: tree TARGET_BUILTIN_RECIPROCAL (tree fndecl)

  .. hook-start:TARGET_BUILTIN_RECIPROCAL

  This hook should return the DECL of a function that implements the
  reciprocal of the machine-specific builtin function :samp:`{fndecl}`, or
  ``NULL_TREE`` if such a function is not available.

.. hook-end

.. function:: tree TARGET_VECTORIZE_BUILTIN_MASK_FOR_LOAD (void)

  .. hook-start:TARGET_VECTORIZE_BUILTIN_MASK_FOR_LOAD

  This hook should return the DECL of a function :samp:`{f}` that given an
  address :samp:`{addr}` as an argument returns a mask :samp:`{m}` that can be
  used to extract from two vectors the relevant data that resides in
  :samp:`{addr}` in case :samp:`{addr}` is not properly aligned.

  The autovectorizer, when vectorizing a load operation from an address
  :samp:`{addr}` that may be unaligned, will generate two vector loads from
  the two aligned addresses around :samp:`{addr}`. It then generates a
  ``REALIGN_LOAD`` operation to extract the relevant data from the
  two loaded vectors. The first two arguments to ``REALIGN_LOAD``,
  :samp:`{v1}` and :samp:`{v2}`, are the two vectors, each of size :samp:`{VS}`, and
  the third argument, :samp:`{OFF}`, defines how the data will be extracted
  from these two vectors: if :samp:`{OFF}` is 0, then the returned vector is
  :samp:`{v2}` ; otherwise, the returned vector is composed from the last
  :samp:`{VS}` - :samp:`{OFF}` elements of :samp:`{v1}` concatenated to the first
  :samp:`{OFF}` elements of :samp:`{v2}`.

  If this hook is defined, the autovectorizer will generate a call
  to :samp:`{f}` (using the DECL tree that this hook returns) and will
  use the return value of :samp:`{f}` as the argument :samp:`{OFF}` to
  ``REALIGN_LOAD``. Therefore, the mask :samp:`{m}` returned by :samp:`{f}`
  should comply with the semantics expected by ``REALIGN_LOAD``
  described above.
  If this hook is not defined, then :samp:`{addr}` will be used as
  the argument :samp:`{OFF}` to ``REALIGN_LOAD``, in which case the low
  log2(:samp:`{VS}`) - 1 bits of :samp:`{addr}` will be considered.

.. hook-end

.. function:: int TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST (enum vect_cost_for_stmt type_of_cost, tree vectype, int misalign)

  .. hook-start:TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST

  Returns cost of different scalar or vector statements for vectorization cost model.
  For vector memory operations the cost may depend on type (:samp:`{vectype}`) and
  misalignment value (:samp:`{misalign}`).

.. hook-end

.. function:: poly_uint64 TARGET_VECTORIZE_PREFERRED_VECTOR_ALIGNMENT (const_tree type)

  .. hook-start:TARGET_VECTORIZE_PREFERRED_VECTOR_ALIGNMENT

  This hook returns the preferred alignment in bits for accesses to
  vectors of type :samp:`{type}` in vectorized code.  This might be less than
  or greater than the ABI-defined value returned by
  ``TARGET_VECTOR_ALIGNMENT``.  It can be equal to the alignment of
  a single element, in which case the vectorizer will not try to optimize
  for alignment.

  The default hook returns ``TYPE_ALIGN (type)``, which is
  correct for most targets.

.. hook-end

.. function:: bool TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE (const_tree type, bool is_packed)

  .. hook-start:TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE

  Return true if vector alignment is reachable (by peeling N iterations)
  for the given scalar type :samp:`{type}`.  :samp:`{is_packed}` is false if the scalar
  access using :samp:`{type}` is known to be naturally aligned.

.. hook-end

.. function:: bool TARGET_VECTORIZE_VEC_PERM_CONST (machine_mode mode, machine_mode op_mode, rtx output, rtx in0, rtx in1, const vec_perm_indices &sel)

  .. hook-start:TARGET_VECTORIZE_VEC_PERM_CONST

  This hook is used to test whether the target can permute up to two
  vectors of mode :samp:`{op_mode}` using the permutation vector ``sel``,
  producing a vector of mode :samp:`{mode}`.  The hook is also used to emit such
  a permutation.

  When the hook is being used to test whether the target supports a permutation,
  :samp:`{in0}`, :samp:`{in1}`, and :samp:`{out}` are all null.  When the hook is being used
  to emit a permutation, :samp:`{in0}` and :samp:`{in1}` are the source vectors of mode
  :samp:`{op_mode}` and :samp:`{out}` is the destination vector of mode :samp:`{mode}`.
  :samp:`{in1}` is the same as :samp:`{in0}` if :samp:`{sel}` describes a permutation on one
  vector instead of two.

  Return true if the operation is possible, emitting instructions for it
  if rtxes are provided.

  .. index:: vec_permm instruction pattern

  If the hook returns false for a mode with multibyte elements, GCC will
  try the equivalent byte operation.  If that also fails, it will try forcing
  the selector into a register and using the :samp:`{vec_perm {mode} }`
  instruction pattern.  There is no need for the hook to handle these two
  implementation approaches itself.

.. hook-end

.. function:: tree TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION (unsigned code, tree vec_type_out, tree vec_type_in)

  .. hook-start:TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION

  This hook should return the decl of a function that implements the
  vectorized variant of the function with the ``combined_fn`` code
  :samp:`{code}` or ``NULL_TREE`` if such a function is not available.
  The return type of the vectorized function shall be of vector type
  :samp:`{vec_type_out}` and the argument types should be :samp:`{vec_type_in}`.

.. hook-end

.. function:: tree TARGET_VECTORIZE_BUILTIN_MD_VECTORIZED_FUNCTION (tree fndecl, tree vec_type_out, tree vec_type_in)

  .. hook-start:TARGET_VECTORIZE_BUILTIN_MD_VECTORIZED_FUNCTION

  This hook should return the decl of a function that implements the
  vectorized variant of target built-in function ``fndecl``.  The
  return type of the vectorized function shall be of vector type
  :samp:`{vec_type_out}` and the argument types should be :samp:`{vec_type_in}`.

.. hook-end

.. function:: bool TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT (machine_mode mode, const_tree type, int misalignment, bool is_packed)

  .. hook-start:TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT

  This hook should return true if the target supports misaligned vector
  store/load of a specific factor denoted in the :samp:`{misalignment}`
  parameter.  The vector store/load should be of machine mode :samp:`{mode}` and
  the elements in the vectors should be of type :samp:`{type}`.  :samp:`{is_packed}`
  parameter is true if the memory access is defined in a packed struct.

.. hook-end

.. function:: machine_mode TARGET_VECTORIZE_PREFERRED_SIMD_MODE (scalar_mode mode)

  .. hook-start:TARGET_VECTORIZE_PREFERRED_SIMD_MODE

  This hook should return the preferred mode for vectorizing scalar
  mode :samp:`{mode}`.  The default is
  equal to ``word_mode``, because the vectorizer can do some
  transformations even in absence of specialized SIMD hardware.

.. hook-end

.. function:: machine_mode TARGET_VECTORIZE_SPLIT_REDUCTION (machine_mode)

  .. hook-start:TARGET_VECTORIZE_SPLIT_REDUCTION

  This hook should return the preferred mode to split the final reduction
  step on :samp:`{mode}` to.  The reduction is then carried out reducing upper
  against lower halves of vectors recursively until the specified mode is
  reached.  The default is :samp:`{mode}` which means no splitting.

.. hook-end

.. function:: unsigned int TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES (vector_modes *modes, bool all)

  .. hook-start:TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES

  If using the mode returned by ``TARGET_VECTORIZE_PREFERRED_SIMD_MODE``
  is not the only approach worth considering, this hook should add one mode to
  :samp:`{modes}` for each useful alternative approach.  These modes are then
  passed to ``TARGET_VECTORIZE_RELATED_MODE`` to obtain the vector mode
  for a given element mode.

  The modes returned in :samp:`{modes}` should use the smallest element mode
  possible for the vectorization approach that they represent, preferring
  integer modes over floating-poing modes in the event of a tie.  The first
  mode should be the ``TARGET_VECTORIZE_PREFERRED_SIMD_MODE`` for its
  element mode.

  If :samp:`{all}` is true, add suitable vector modes even when they are generally
  not expected to be worthwhile.

  The hook returns a bitmask of flags that control how the modes in
  :samp:`{modes}` are used.  The flags are:

  .. envvar:: VECT_COMPARE_COSTS

    Tells the loop vectorizer to try all the provided modes and pick the one
    with the lowest cost.  By default the vectorizer will choose the first
    mode that works.

  The hook does not need to do anything if the vector returned by
  ``TARGET_VECTORIZE_PREFERRED_SIMD_MODE`` is the only one relevant
  for autovectorization.  The default implementation adds no modes and
  returns 0.

.. hook-end

.. function:: opt_machine_mode TARGET_VECTORIZE_RELATED_MODE (machine_mode vector_mode, scalar_mode element_mode, poly_uint64 nunits)

  .. hook-start:TARGET_VECTORIZE_RELATED_MODE

  If a piece of code is using vector mode :samp:`{vector_mode}` and also wants
  to operate on elements of mode :samp:`{element_mode}`, return the vector mode
  it should use for those elements.  If :samp:`{nunits}` is nonzero, ensure that
  the mode has exactly :samp:`{nunits}` elements, otherwise pick whichever vector
  size pairs the most naturally with :samp:`{vector_mode}`.  Return an empty
  ``opt_machine_mode`` if there is no supported vector mode with the
  required properties.

  There is no prescribed way of handling the case in which :samp:`{nunits}`
  is zero.  One common choice is to pick a vector mode with the same size
  as :samp:`{vector_mode}` ; this is the natural choice if the target has a
  fixed vector size.  Another option is to choose a vector mode with the
  same number of elements as :samp:`{vector_mode}` ; this is the natural choice
  if the target has a fixed number of elements.  Alternatively, the hook
  might choose a middle ground, such as trying to keep the number of
  elements as similar as possible while applying maximum and minimum
  vector sizes.

  The default implementation uses ``mode_for_vector`` to find the
  requested mode, returning a mode with the same size as :samp:`{vector_mode}`
  when :samp:`{nunits}` is zero.  This is the correct behavior for most targets.

.. hook-end

.. function:: opt_machine_mode TARGET_VECTORIZE_GET_MASK_MODE (machine_mode mode)

  .. hook-start:TARGET_VECTORIZE_GET_MASK_MODE

  Return the mode to use for a vector mask that holds one boolean
  result for each element of vector mode :samp:`{mode}`.  The returned mask mode
  can be a vector of integers (class ``MODE_VECTOR_INT``), a vector of
  booleans (class ``MODE_VECTOR_BOOL``) or a scalar integer (class
  ``MODE_INT``).  Return an empty ``opt_machine_mode`` if no such
  mask mode exists.

  The default implementation returns a ``MODE_VECTOR_INT`` with the
  same size and number of elements as :samp:`{mode}`, if such a mode exists.

.. hook-end

.. function:: bool TARGET_VECTORIZE_EMPTY_MASK_IS_EXPENSIVE (unsigned ifn)

  .. hook-start:TARGET_VECTORIZE_EMPTY_MASK_IS_EXPENSIVE

  This hook returns true if masked internal function :samp:`{ifn}` (really of
  type ``internal_fn``) should be considered expensive when the mask is
  all zeros.  GCC can then try to branch around the instruction instead.

.. hook-end

.. function:: class vector_costs * TARGET_VECTORIZE_CREATE_COSTS (vec_info *vinfo, bool costing_for_scalar)

  .. hook-start:TARGET_VECTORIZE_CREATE_COSTS

  This hook should initialize target-specific data structures in preparation
  for modeling the costs of vectorizing a loop or basic block.  The default
  allocates three unsigned integers for accumulating costs for the prologue,
  body, and epilogue of the loop or basic block.  If :samp:`{loop_info}` is
  non-NULL, it identifies the loop being vectorized; otherwise a single block
  is being vectorized.  If :samp:`{costing_for_scalar}` is true, it indicates the
  current cost model is for the scalar version of a loop or block; otherwise
  it is for the vector version.

.. hook-end

.. function:: tree TARGET_VECTORIZE_BUILTIN_GATHER (const_tree mem_vectype, const_tree index_type, int scale)

  .. hook-start:TARGET_VECTORIZE_BUILTIN_GATHER

  Target builtin that implements vector gather operation.  :samp:`{mem_vectype}`
  is the vector type of the load and :samp:`{index_type}` is scalar type of
  the index, scaled by :samp:`{scale}`.
  The default is ``NULL_TREE`` which means to not vectorize gather
  loads.

.. hook-end

.. function:: tree TARGET_VECTORIZE_BUILTIN_SCATTER (const_tree vectype, const_tree index_type, int scale)

  .. hook-start:TARGET_VECTORIZE_BUILTIN_SCATTER

  Target builtin that implements vector scatter operation.  :samp:`{vectype}`
  is the vector type of the store and :samp:`{index_type}` is scalar type of
  the index, scaled by :samp:`{scale}`.
  The default is ``NULL_TREE`` which means to not vectorize scatter
  stores.

.. hook-end

.. function:: int TARGET_SIMD_CLONE_COMPUTE_VECSIZE_AND_SIMDLEN (struct cgraph_node *, struct cgraph_simd_clone *, tree, int)

  .. hook-start:TARGET_SIMD_CLONE_COMPUTE_VECSIZE_AND_SIMDLEN

  This hook should set :samp:`{vecsize_mangle}`, :samp:`{vecsize_int}`, :samp:`{vecsize_float}`
  fields in :samp:`{simd_clone}` structure pointed by :samp:`{clone_info}` argument and also
  :samp:`{simdlen}` field if it was previously 0.
  :samp:`{vecsize_mangle}` is a marker for the backend only. :samp:`{vecsize_int}` and
  :samp:`{vecsize_float}` should be left zero on targets where the number of lanes is
  not determined by the bitsize (in which case :samp:`{simdlen}` is always used).
  The hook should return 0 if SIMD clones shouldn't be emitted,
  or number of :samp:`{vecsize_mangle}` variants that should be emitted.

.. hook-end

.. function:: void TARGET_SIMD_CLONE_ADJUST (struct cgraph_node *)

  .. hook-start:TARGET_SIMD_CLONE_ADJUST

  This hook should add implicit ``attribute(target("..."))`` attribute
  to SIMD clone :samp:`{node}` if needed.

.. hook-end

.. function:: int TARGET_SIMD_CLONE_USABLE (struct cgraph_node *)

  .. hook-start:TARGET_SIMD_CLONE_USABLE

  This hook should return -1 if SIMD clone :samp:`{node}` shouldn't be used
  in vectorized loops in current function, or non-negative number if it is
  usable.  In that case, the smaller the number is, the more desirable it is
  to use it.

.. hook-end

.. function:: int TARGET_SIMT_VF (void)

  .. hook-start:TARGET_SIMT_VF

  Return number of threads in SIMT thread group on the target.

.. hook-end

.. function:: int TARGET_OMP_DEVICE_KIND_ARCH_ISA (enum omp_device_kind_arch_isa trait, const char *name)

  .. hook-start:TARGET_OMP_DEVICE_KIND_ARCH_ISA

  Return 1 if :samp:`{trait}` :samp:`{name}` is present in the OpenMP context's
  device trait set, return 0 if not present in any OpenMP context in the
  whole translation unit, or -1 if not present in the current OpenMP context
  but might be present in another OpenMP context in the same TU.

.. hook-end

.. function:: bool TARGET_GOACC_VALIDATE_DIMS (tree decl, int *dims, int fn_level, unsigned used)

  .. hook-start:TARGET_GOACC_VALIDATE_DIMS

  This hook should check the launch dimensions provided for an OpenACC
  compute region, or routine.  Defaulted values are represented as -1
  and non-constant values as 0.  The :samp:`{fn_level}` is negative for the
  function corresponding to the compute region.  For a routine it is the
  outermost level at which partitioned execution may be spawned.  The hook
  should verify non-default values.  If DECL is NULL, global defaults
  are being validated and unspecified defaults should be filled in.
  Diagnostics should be issued as appropriate.  Return
  true, if changes have been made.  You must override this hook to
  provide dimensions larger than 1.

.. hook-end

.. function:: int TARGET_GOACC_DIM_LIMIT (int axis)

  .. hook-start:TARGET_GOACC_DIM_LIMIT

  This hook should return the maximum size of a particular dimension,
  or zero if unbounded.

.. hook-end

.. function:: bool TARGET_GOACC_FORK_JOIN (gcall *call, const int *dims, bool is_fork)

  .. hook-start:TARGET_GOACC_FORK_JOIN

  This hook can be used to convert IFN_GOACC_FORK and IFN_GOACC_JOIN
  function calls to target-specific gimple, or indicate whether they
  should be retained.  It is executed during the oacc_device_lower pass.
  It should return true, if the call should be retained.  It should
  return false, if it is to be deleted (either because target-specific
  gimple has been inserted before it, or there is no need for it).
  The default hook returns false, if there are no RTL expanders for them.

.. hook-end

.. function:: void TARGET_GOACC_REDUCTION (gcall *call)

  .. hook-start:TARGET_GOACC_REDUCTION

  This hook is used by the oacc_transform pass to expand calls to the
  :samp:`{GOACC_REDUCTION}` internal function, into a sequence of gimple
  instructions.  :samp:`{call}` is gimple statement containing the call to
  the function.  This hook removes statement :samp:`{call}` after the
  expanded sequence has been inserted.  This hook is also responsible
  for allocating any storage for reductions when necessary.

.. hook-end

.. function:: tree TARGET_PREFERRED_ELSE_VALUE (unsigned ifn, tree type, unsigned nops, tree *ops)

  .. hook-start:TARGET_PREFERRED_ELSE_VALUE

  This hook returns the target's preferred final argument for a call
  to conditional internal function :samp:`{ifn}` (really of type
  ``internal_fn``).  :samp:`{type}` specifies the return type of the
  function and :samp:`{ops}` are the operands to the conditional operation,
  of which there are :samp:`{nops}`.

  For example, if :samp:`{ifn}` is ``IFN_COND_ADD``, the hook returns
  a value of type :samp:`{type}` that should be used when :samp:`{ops}[0]`
  and :samp:`{ops}[1]` are conditionally added together.

  This hook is only relevant if the target supports conditional patterns
  like ``cond_addm``.  The default implementation returns a zero
  constant of type :samp:`{type}`.

.. hook-end

.. function:: tree TARGET_GOACC_ADJUST_PRIVATE_DECL (location_t loc, tree var, int level)

  .. hook-start:TARGET_GOACC_ADJUST_PRIVATE_DECL

  This hook, if defined, is used by accelerator target back-ends to adjust
  OpenACC variable declarations that should be made private to the given
  parallelism level (i.e. ``GOMP_DIM_GANG``, ``GOMP_DIM_WORKER`` or
  ``GOMP_DIM_VECTOR``).  A typical use for this hook is to force variable
  declarations at the ``gang`` level to reside in GPU shared memory.
  :samp:`{loc}` may be used for diagnostic purposes.

  You may also use the ``TARGET_GOACC_EXPAND_VAR_DECL`` hook if the
  adjusted variable declaration needs to be expanded to RTL in a non-standard
  way.

.. hook-end

.. function:: rtx TARGET_GOACC_EXPAND_VAR_DECL (tree var)

  .. hook-start:TARGET_GOACC_EXPAND_VAR_DECL

  This hook, if defined, is used by accelerator target back-ends to expand
  specially handled kinds of ``VAR_DECL`` expressions.  A particular use is
  to place variables with specific attributes inside special accelarator
  memories.  A return value of ``NULL`` indicates that the target does not
  handle this ``VAR_DECL``, and normal RTL expanding is resumed.

  Only define this hook if your accelerator target needs to expand certain
  ``VAR_DECL`` nodes in a way that differs from the default.  You can also adjust
  private variables at OpenACC device-lowering time using the
  ``TARGET_GOACC_ADJUST_PRIVATE_DECL`` target hook.

.. hook-end

.. function:: tree TARGET_GOACC_CREATE_WORKER_BROADCAST_RECORD (tree rec, bool sender, const char *name, unsigned HOST_WIDE_INT offset)

  .. hook-start:TARGET_GOACC_CREATE_WORKER_BROADCAST_RECORD

  Create a record used to propagate local-variable state from an active
  worker to other workers.  A possible implementation might adjust the type
  of REC to place the new variable in shared GPU memory.

  Presence of this target hook indicates that middle end neutering/broadcasting
  be used.
.. hook-end

.. function:: void TARGET_GOACC_SHARED_MEM_LAYOUT (unsigned HOST_WIDE_INT *, unsigned HOST_WIDE_INT *, int[], unsigned HOST_WIDE_INT[], unsigned HOST_WIDE_INT[])

  .. hook-start:TARGET_GOACC_SHARED_MEM_LAYOUT

  Lay out a fixed shared-memory region on the target.  The LO and HI
  arguments should be set to a range of addresses that can be used for worker
  broadcasting. The dimensions, reduction size and gang-private size
  arguments are for the current offload region.

.. hook-end