..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: alias, flow-sensitive alias analysis, flow-insensitive alias analysis

.. _alias-analysis:

Alias analysis
**************

Alias analysis in GIMPLE SSA form consists of two pieces.  First
the virtual SSA web ties conflicting memory accesses and provides
a SSA use-def chain and SSA immediate-use chains for walking
possibly dependent memory accesses.  Second an alias-oracle can
be queried to disambiguate explicit and implicit memory references.

* Memory SSA form.

  All statements that may use memory have exactly one accompanied use of
  a virtual SSA name that represents the state of memory at the
  given point in the IL.

  All statements that may define memory have exactly one accompanied
  definition of a virtual SSA name using the previous state of memory
  and defining the new state of memory after the given point in the IL.

  .. code-block:: c++

    int i;
    int foo (void)
    {
      # .MEM_3 = VDEF <.MEM_2(D)>
      i = 1;
      # VUSE <.MEM_3>
      return i;
    }

  The virtual SSA names in this case are ``.MEM_2(D)`` and
  ``.MEM_3``.  The store to the global variable ``i``
  defines ``.MEM_3`` invalidating ``.MEM_2(D)``.  The
  load from ``i`` uses that new state ``.MEM_3``.

  The virtual SSA web serves as constraints to SSA optimizers
  preventing illegitimate code-motion and optimization.  It
  also provides a way to walk related memory statements.

* Points-to and escape analysis.

  Points-to analysis builds a set of constraints from the GIMPLE
  SSA IL representing all pointer operations and facts we do
  or do not know about pointers.  Solving this set of constraints
  yields a conservatively correct solution for each pointer
  variable in the program (though we are only interested in
  SSA name pointers) as to what it may possibly point to.

  This points-to solution for a given SSA name pointer is stored
  in the ``pt_solution`` sub-structure of the
  ``SSA_NAME_PTR_INFO`` record.  The following accessor
  functions are available:

  * ``pt_solution_includes``

  * ``pt_solutions_intersect``

  Points-to analysis also computes the solution for two special
  set of pointers, ``ESCAPED`` and ``CALLUSED``.  Those
  represent all memory that has escaped the scope of analysis
  or that is used by pure or nested const calls.

* Type-based alias analysis

  Type-based alias analysis is frontend dependent though generic
  support is provided by the middle-end in ``alias.cc``.  TBAA
  code is used by both tree optimizers and RTL optimizers.

  Every language that wishes to perform language-specific alias analysis
  should define a function that computes, given a ``tree``
  node, an alias set for the node.  Nodes in different alias sets are not
  allowed to alias.  For an example, see the C front-end function
  ``c_get_alias_set``.

* Tree alias-oracle

  The tree alias-oracle provides means to disambiguate two memory
  references and memory references against statements.  The following
  queries are available:

  * ``refs_may_alias_p``

  * ``ref_maybe_used_by_stmt_p``

  * ``stmt_may_clobber_ref_p``

  In addition to those two kind of statement walkers are available
  walking statements related to a reference ref.
  ``walk_non_aliased_vuses`` walks over dominating memory defining
  statements and calls back if the statement does not clobber ref
  providing the non-aliased VUSE.  The walk stops at
  the first clobbering statement or if asked to.
  ``walk_aliased_vdefs`` walks over dominating memory defining
  statements and calls back on each statement clobbering ref
  providing its aliasing VDEF.  The walk stops if asked to.
