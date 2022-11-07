..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE

.. _gimple:

GIMPLE
------

GIMPLE is a three-address representation derived from GENERIC by
breaking down GENERIC expressions into tuples of no more than 3
operands (with some exceptions like function calls).  GIMPLE was
heavily influenced by the SIMPLE IL used by the McCAT compiler
project at McGill University, though we have made some different
choices.  For one thing, SIMPLE doesn't support ``goto``.

Temporaries are introduced to hold intermediate values needed to
compute complex expressions. Additionally, all the control
structures used in GENERIC are lowered into conditional jumps,
lexical scopes are removed and exception regions are converted
into an on the side exception region tree.

The compiler pass which converts GENERIC into GIMPLE is referred to as
the :samp:`gimplifier`.  The gimplifier works recursively, generating
GIMPLE tuples out of the original GENERIC expressions.

One of the early implementation strategies used for the GIMPLE
representation was to use the same internal data structures used
by front ends to represent parse trees. This simplified
implementation because we could leverage existing functionality
and interfaces. However, GIMPLE is a much more restrictive
representation than abstract syntax trees (AST), therefore it
does not require the full structural complexity provided by the
main tree data structure.

The GENERIC representation of a function is stored in the
``DECL_SAVED_TREE`` field of the associated ``FUNCTION_DECL``
tree node.  It is converted to GIMPLE by a call to
``gimplify_function_tree``.

If a front end wants to include language-specific tree codes in the tree
representation which it provides to the back end, it must provide a
definition of ``LANG_HOOKS_GIMPLIFY_EXPR`` which knows how to
convert the front end trees to GIMPLE.  Usually such a hook will involve
much of the same code for expanding front end trees to RTL.  This function
can return fully lowered GIMPLE, or it can return GENERIC trees and let the
main gimplifier lower them the rest of the way; this is often simpler.
GIMPLE that is not fully lowered is known as 'High GIMPLE' and
consists of the IL before the pass ``pass_lower_cf``.  High GIMPLE
contains some container statements like lexical scopes
(represented by ``GIMPLE_BIND``) and nested expressions (e.g.,
``GIMPLE_TRY``), while 'Low GIMPLE' exposes all of the
implicit jumps for control and exception expressions directly in
the IL and EH region trees.

The C and C++ front ends currently convert directly from front end
trees to GIMPLE, and hand that off to the back end rather than first
converting to GENERIC.  Their gimplifier hooks know about all the
``_STMT`` nodes and how to convert them to GENERIC forms.  There
was some work done on a genericization pass which would run first, but
the existence of ``STMT_EXPR`` meant that in order to convert all
of the C statements into GENERIC equivalents would involve walking the
entire tree anyway, so it was simpler to lower all the way.  This
might change in the future if someone writes an optimization pass
which would work better with higher-level trees, but currently the
optimizers all expect GIMPLE.

You can request to dump a C-like representation of the GIMPLE form
with the flag :option:`-fdump-tree-gimple`.

.. toctree::
  :maxdepth: 2

  gimple/tuple-representation
  gimple/class-hierarchy-of-gimple-statements
  gimple/gimple-instruction-set
  gimple/temporaries
  gimple/operands
  gimple/manipulating-gimple-statements
  gimple/tuple-specific-accessors
  gimple/gimple-sequences
  gimple/sequence-iterators
  gimple/adding-a-new-gimple-statement-code
  gimple/statement-and-operand-traversals
  gimple/exception-handling