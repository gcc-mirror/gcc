..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_PHI

GIMPLE_PHI
^^^^^^^^^^

.. function:: unsigned gimple_phi_capacity (gimple g)

  Return the maximum number of arguments supported by ``GIMPLE_PHI`` ``G``.

.. function:: unsigned gimple_phi_num_args (gimple g)

  Return the number of arguments in ``GIMPLE_PHI`` ``G``. This must always
  be exactly the number of incoming edges for the basic block
  holding ``G``.

.. function:: tree gimple_phi_result (gimple g)

  Return the ``SSA`` name created by ``GIMPLE_PHI`` ``G``.

.. function:: tree * gimple_phi_result_ptr (gimple g)

  Return a pointer to the ``SSA`` name created by ``GIMPLE_PHI`` ``G``.

.. function:: void gimple_phi_set_result (gphi *g, tree result)

  Set ``RESULT`` to be the ``SSA`` name created by ``GIMPLE_PHI`` ``G``.

.. function:: struct phi_arg_d * gimple_phi_arg (gimple g, index)

  Return the ``PHI`` argument corresponding to incoming edge ``INDEX`` for
  ``GIMPLE_PHI`` ``G``.

.. function:: void gimple_phi_set_arg (gphi *g, index, struct phi_arg_d * phiarg)

  Set ``PHIARG`` to be the argument corresponding to incoming edge
  ``INDEX`` for ``GIMPLE_PHI`` ``G``.