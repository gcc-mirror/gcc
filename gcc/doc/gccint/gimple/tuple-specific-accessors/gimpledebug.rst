..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_DEBUG, GIMPLE_DEBUG_BIND, GIMPLE_DEBUG_BEGIN_STMT, GIMPLE_DEBUG_INLINE_ENTRY

.. _gimple_debug:

GIMPLE_DEBUG
^^^^^^^^^^^^

.. function:: gdebug *gimple_build_debug_bind (tree var, tree value, gimple stmt)

  Build a ``GIMPLE_DEBUG`` statement with ``GIMPLE_DEBUG_BIND``
  ``subcode``.  The effect of this statement is to tell debug
  information generation machinery that the value of user variable
  ``var`` is given by ``value`` at that point, and to remain with
  that value until ``var`` runs out of scope, a
  dynamically-subsequent debug bind statement overrides the binding, or
  conflicting values reach a control flow merge point.  Even if
  components of the ``value`` expression change afterwards, the
  variable is supposed to retain the same value, though not necessarily
  the same location.

  It is expected that ``var`` be most often a tree for automatic user
  variables (``VAR_DECL`` or ``PARM_DECL``) that satisfy the
  requirements for gimple registers, but it may also be a tree for a
  scalarized component of a user variable (``ARRAY_REF``,
  ``COMPONENT_REF``), or a debug temporary (``DEBUG_EXPR_DECL``).

  As for ``value``, it can be an arbitrary tree expression, but it is
  recommended that it be in a suitable form for a gimple assignment
  ``RHS``.  It is not expected that user variables that could appear
  as ``var`` ever appear in ``value``, because in the latter we'd
  have their ``SSA_NAME`` s instead, but even if they were not in SSA
  form, user variables appearing in ``value`` are to be regarded as
  part of the executable code space, whereas those in ``var`` are to
  be regarded as part of the source code space.  There is no way to
  refer to the value bound to a user variable within a ``value``
  expression.

  If ``value`` is ``GIMPLE_DEBUG_BIND_NOVALUE``, debug information
  generation machinery is informed that the variable ``var`` is
  unbound, i.e., that its value is indeterminate, which sometimes means
  it is really unavailable, and other times that the compiler could not
  keep track of it.

  Block and location information for the newly-created stmt are
  taken from ``stmt``, if given.

.. function:: tree gimple_debug_bind_get_var (gimple stmt)

  Return the user variable :samp:`{var}` that is bound at ``stmt``.

.. function:: tree gimple_debug_bind_get_value (gimple stmt)

  Return the value expression that is bound to a user variable at
  ``stmt``.

.. function:: tree * gimple_debug_bind_get_value_ptr (gimple stmt)

  Return a pointer to the value expression that is bound to a user
  variable at ``stmt``.

.. function:: void gimple_debug_bind_set_var (gimple stmt, tree var)

  Modify the user variable bound at ``stmt`` to :samp:`{var}`.

.. function:: void gimple_debug_bind_set_value (gimple stmt, tree var)

  Modify the value bound to the user variable bound at ``stmt`` to
  :samp:`{value}`.

.. function:: void gimple_debug_bind_reset_value (gimple stmt)

  Modify the value bound to the user variable bound at ``stmt`` so
  that the variable becomes unbound.

.. function:: bool gimple_debug_bind_has_value_p (gimple stmt)

  Return ``TRUE`` if ``stmt`` binds a user variable to a value,
  and ``FALSE`` if it unbinds the variable.

.. function:: gimple gimple_build_debug_begin_stmt (tree block, location_t location)

  Build a ``GIMPLE_DEBUG`` statement with
  ``GIMPLE_DEBUG_BEGIN_STMT`` ``subcode``.  The effect of this
  statement is to tell debug information generation machinery that the
  user statement at the given ``location`` and ``block`` starts at
  the point at which the statement is inserted.  The intent is that side
  effects (e.g. variable bindings) of all prior user statements are
  observable, and that none of the side effects of subsequent user
  statements are.

.. function:: gimple gimple_build_debug_inline_entry (tree block, location_t location)

  Build a ``GIMPLE_DEBUG`` statement with
  ``GIMPLE_DEBUG_INLINE_ENTRY`` ``subcode``.  The effect of this
  statement is to tell debug information generation machinery that a
  function call at ``location`` underwent inline substitution, that
  ``block`` is the enclosing lexical block created for the
  substitution, and that at the point of the program in which the stmt is
  inserted, all parameters for the inlined function are bound to the
  respective arguments, and none of the side effects of its stmts are
  observable.