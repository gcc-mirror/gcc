..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Manipulating GIMPLE statements

.. _manipulating-gimple-statements:

Manipulating GIMPLE statements
******************************

This section documents all the functions available to handle each
of the GIMPLE instructions.

Common accessors
^^^^^^^^^^^^^^^^

The following are common accessors for gimple statements.

.. function:: enum gimple_code gimple_code (gimple g)

  Return the code for statement ``G``.

.. function:: basic_block gimple_bb (gimple g)

  Return the basic block to which statement ``G`` belongs to.

.. function:: tree gimple_block (gimple g)

  Return the lexical scope block holding statement ``G``.

.. function:: enum tree_code gimple_expr_code (gimple stmt)

  Return the tree code for the expression computed by ``STMT``.  This
  is only meaningful for ``GIMPLE_CALL``, ``GIMPLE_ASSIGN`` and
  ``GIMPLE_COND``.  If ``STMT`` is ``GIMPLE_CALL``, it will return ``CALL_EXPR``.
  For ``GIMPLE_COND``, it returns the code of the comparison predicate.
  For ``GIMPLE_ASSIGN`` it returns the code of the operation performed
  by the ``RHS`` of the assignment.

.. function:: void gimple_set_block (gimple g, tree block)

  Set the lexical scope block of ``G`` to ``BLOCK``.

.. function:: location_t gimple_locus (gimple g)

  Return locus information for statement ``G``.

.. function:: void gimple_set_locus (gimple g, location_t locus)

  Set locus information for statement ``G``.

.. function:: bool gimple_locus_empty_p (gimple g)

  Return true if ``G`` does not have locus information.

.. function:: bool gimple_no_warning_p (gimple stmt)

  Return true if no warnings should be emitted for statement ``STMT``.

.. function:: void gimple_set_visited (gimple stmt, bool visited_p)

  Set the visited status on statement ``STMT`` to ``VISITED_P``.

.. function:: bool gimple_visited_p (gimple stmt)

  Return the visited status on statement ``STMT``.

.. function:: void gimple_set_plf (gimple stmt, enum plf_mask plf, bool val_p)

  Set pass local flag ``PLF`` on statement ``STMT`` to ``VAL_P``.

.. function:: unsigned int gimple_plf (gimple stmt, enum plf_mask plf)

  Return the value of pass local flag ``PLF`` on statement ``STMT``.

.. function:: bool gimple_has_ops (gimple g)

  Return true if statement ``G`` has register or memory operands.

.. function:: bool gimple_has_mem_ops (gimple g)

  Return true if statement ``G`` has memory operands.

.. function:: unsigned gimple_num_ops (gimple g)

  Return the number of operands for statement ``G``.

.. function:: tree * gimple_ops (gimple g)

  Return the array of operands for statement ``G``.

.. function:: tree gimple_op (gimple g, unsigned i)

  Return operand ``I`` for statement ``G``.

.. function:: tree * gimple_op_ptr (gimple g, unsigned i)

  Return a pointer to operand ``I`` for statement ``G``.

.. function:: void gimple_set_op (gimple g, unsigned i, tree op)

  Set operand ``I`` of statement ``G`` to ``OP``.

.. function:: bitmap gimple_addresses_taken (gimple stmt)

  Return the set of symbols that have had their address taken by
  ``STMT``.

.. function:: struct def_optype_d * gimple_def_ops (gimple g)

  Return the set of ``DEF`` operands for statement ``G``.

.. function:: void gimple_set_def_ops (gimple g, struct def_optype_d *def)

  Set ``DEF`` to be the set of ``DEF`` operands for statement ``G``.

.. function:: struct use_optype_d * gimple_use_ops (gimple g)

  Return the set of ``USE`` operands for statement ``G``.

.. function:: void gimple_set_use_ops (gimple g, struct use_optype_d *use)

  Set ``USE`` to be the set of ``USE`` operands for statement ``G``.

.. function:: struct voptype_d * gimple_vuse_ops (gimple g)

  Return the set of ``VUSE`` operands for statement ``G``.

.. function:: void gimple_set_vuse_ops (gimple g, struct voptype_d *ops)

  Set ``OPS`` to be the set of ``VUSE`` operands for statement ``G``.

.. function:: struct voptype_d * gimple_vdef_ops (gimple g)

  Return the set of ``VDEF`` operands for statement ``G``.

.. function:: void gimple_set_vdef_ops (gimple g, struct voptype_d *ops)

  Set ``OPS`` to be the set of ``VDEF`` operands for statement ``G``.

.. function:: bitmap gimple_loaded_syms (gimple g)

  Return the set of symbols loaded by statement ``G``.  Each element of
  the set is the ``DECL_UID`` of the corresponding symbol.

.. function:: bitmap gimple_stored_syms (gimple g)

  Return the set of symbols stored by statement ``G``.  Each element of
  the set is the ``DECL_UID`` of the corresponding symbol.

.. function:: bool gimple_modified_p (gimple g)

  Return true if statement ``G`` has operands and the modified field
  has been set.

.. function:: bool gimple_has_volatile_ops (gimple stmt)

  Return true if statement ``STMT`` contains volatile operands.

.. function:: void gimple_set_has_volatile_ops (gimple stmt, bool volatilep)

  Return true if statement ``STMT`` contains volatile operands.

.. function:: void update_stmt (gimple s)

  Mark statement ``S`` as modified, and update it.

.. function:: void update_stmt_if_modified (gimple s)

  Update statement ``S`` if it has been marked modified.

.. function:: gimple gimple_copy (gimple stmt)

  Return a deep copy of statement ``STMT``.
