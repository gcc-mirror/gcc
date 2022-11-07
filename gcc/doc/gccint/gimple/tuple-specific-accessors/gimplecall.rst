..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_CALL

GIMPLE_CALL
^^^^^^^^^^^

.. function:: gcall *gimple_build_call (tree fn, unsigned nargs, ...)

  Build a ``GIMPLE_CALL`` statement to function ``FN``.  The argument ``FN``
  must be either a ``FUNCTION_DECL`` or a gimple call address as
  determined by ``is_gimple_call_addr``.  ``NARGS`` are the number of
  arguments.  The rest of the arguments follow the argument ``NARGS``,
  and must be trees that are valid as rvalues in gimple (i.e., each
  operand is validated with ``is_gimple_operand``).

.. function:: gcall *gimple_build_call_from_tree (tree call_expr, tree fnptrtype)

  Build a ``GIMPLE_CALL`` from a ``CALL_EXPR`` node.  The arguments
  and the function are taken from the expression directly.  The type of the
  ``GIMPLE_CALL`` is set from the second parameter passed by a caller.
  This routine assumes that ``call_expr`` is already in GIMPLE form.
  That is, its operands are GIMPLE values and the function call needs no further
  simplification.  All the call flags in ``call_expr`` are copied over
  to the new ``GIMPLE_CALL``.

.. function:: gcall *gimple_build_call_vec (tree fn, vec<tree> args)

  Identical to ``gimple_build_call`` but the arguments are stored in a
  ``vec<tree>``.

.. function:: tree gimple_call_lhs (gimple g)

  Return the ``LHS`` of call statement ``G``.

.. function:: tree * gimple_call_lhs_ptr (gimple g)

  Return a pointer to the ``LHS`` of call statement ``G``.

.. function:: void gimple_call_set_lhs (gimple g, tree lhs)

  Set ``LHS`` to be the ``LHS`` operand of call statement ``G``.

.. function:: tree gimple_call_fn (gimple g)

  Return the tree node representing the function called by call
  statement ``G``.

.. function:: void gimple_call_set_fn (gcall *g, tree fn)

  Set ``FN`` to be the function called by call statement ``G``.  This has
  to be a gimple value specifying the address of the called
  function.

.. function:: tree gimple_call_fndecl (gimple g)

  If a given ``GIMPLE_CALL`` 's callee is a ``FUNCTION_DECL``, return it.
  Otherwise return ``NULL``.  This function is analogous to
  ``get_callee_fndecl`` in ``GENERIC``.

.. function:: tree gimple_call_set_fndecl (gimple g, tree fndecl)

  Set the called function to ``FNDECL``.

.. function:: tree gimple_call_return_type (const gcall *g)

  Return the type returned by call statement ``G``.

.. function:: tree gimple_call_chain (gimple g)

  Return the static chain for call statement ``G``.

.. function:: void gimple_call_set_chain (gcall *g, tree chain)

  Set ``CHAIN`` to be the static chain for call statement ``G``.

.. function:: unsigned gimple_call_num_args (gimple g)

  Return the number of arguments used by call statement ``G``.

.. function:: tree gimple_call_arg (gimple g, unsigned index)

  Return the argument at position ``INDEX`` for call statement ``G``.  The
  first argument is 0.

.. function:: tree * gimple_call_arg_ptr (gimple g, unsigned index)

  Return a pointer to the argument at position ``INDEX`` for call
  statement ``G``.

.. function:: void gimple_call_set_arg (gimple g, unsigned index, tree arg)

  Set ``ARG`` to be the argument at position ``INDEX`` for call statement
  ``G``.

.. function:: void gimple_call_set_tail (gcall *s)

  Mark call statement ``S`` as being a tail call (i.e., a call just
  before the exit of a function). These calls are candidate for
  tail call optimization.

.. function:: bool gimple_call_tail_p (gcall *s)

  Return true if ``GIMPLE_CALL`` ``S`` is marked as a tail call.

.. function:: bool gimple_call_noreturn_p (gimple s)

  Return true if ``S`` is a noreturn call.

.. function:: gimple gimple_call_copy_skip_args (gcall *stmt, bitmap args_to_skip)

  Build a ``GIMPLE_CALL`` identical to ``STMT`` but skipping the arguments
  in the positions marked by the set ``ARGS_TO_SKIP``.