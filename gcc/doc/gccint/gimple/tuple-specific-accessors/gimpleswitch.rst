..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_SWITCH

GIMPLE_SWITCH
^^^^^^^^^^^^^

.. function:: gswitch *gimple_build_switch (tree index, tree default_label, vec<tree> *args)

  Build a ``GIMPLE_SWITCH`` statement.  ``INDEX`` is the index variable
  to switch on, and ``DEFAULT_LABEL`` represents the default label.
  ``ARGS`` is a vector of ``CASE_LABEL_EXPR`` trees that contain the
  non-default case labels.  Each label is a tree of code ``CASE_LABEL_EXPR``.

.. function:: unsigned gimple_switch_num_labels ( const gswitch *g)

  Return the number of labels associated with the switch statement
  ``G``.

.. function:: void gimple_switch_set_num_labels (gswitch *g, unsigned nlabels)

  Set ``NLABELS`` to be the number of labels for the switch statement
  ``G``.

.. function:: tree gimple_switch_index (const gswitch *g)

  Return the index variable used by the switch statement ``G``.

.. function:: void gimple_switch_set_index (gswitch *g, tree index)

  Set ``INDEX`` to be the index variable for switch statement ``G``.

.. function:: tree gimple_switch_label (const gswitch *g, unsigned index)

  Return the label numbered ``INDEX``. The default label is 0, followed
  by any labels in a switch statement.

.. function:: void gimple_switch_set_label (gswitch *g, unsigned index, tree label)

  Set the label number ``INDEX`` to ``LABEL``. 0 is always the default
  label.

.. function:: tree gimple_switch_default_label ( const gswitch *g)

  Return the default label for a switch statement.

.. function:: void gimple_switch_set_default_label (gswitch *g, tree label)

  Set the default label for a switch statement.