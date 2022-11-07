..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_CATCH

GIMPLE_CATCH
^^^^^^^^^^^^

.. function:: gcatch *gimple_build_catch (tree types, gimple_seq handler)

  Build a ``GIMPLE_CATCH`` statement.  ``TYPES`` are the tree types this
  catch handles.  ``HANDLER`` is a sequence of statements with the code
  for the handler.

.. function:: tree gimple_catch_types (const gcatch *g)

  Return the types handled by ``GIMPLE_CATCH`` statement ``G``.

.. function:: tree * gimple_catch_types_ptr (gcatch *g)

  Return a pointer to the types handled by ``GIMPLE_CATCH`` statement
  ``G``.

.. function:: gimple_seq gimple_catch_handler (gcatch *g)

  Return the GIMPLE sequence representing the body of the handler
  of ``GIMPLE_CATCH`` statement ``G``.

.. function:: void gimple_catch_set_types (gcatch *g, tree t)

  Set ``T`` to be the set of types handled by ``GIMPLE_CATCH`` ``G``.

.. function:: void gimple_catch_set_handler (gcatch *g, gimple_seq handler)

  Set ``HANDLER`` to be the body of ``GIMPLE_CATCH`` ``G``.