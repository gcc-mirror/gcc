..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: gimplification, GIMPLE

.. _gimplification-pass:

Gimplification pass
*******************

:dfn:`Gimplification` is a whimsical term for the process of converting
the intermediate representation of a function into the GIMPLE language
(see :ref:`gimple`).  The term stuck, and so words like 'gimplification',
'gimplify', 'gimplifier' and the like are sprinkled throughout this
section of code.

While a front end may certainly choose to generate GIMPLE directly if
it chooses, this can be a moderately complex process unless the
intermediate language used by the front end is already fairly simple.
Usually it is easier to generate GENERIC trees plus extensions
and let the language-independent gimplifier do most of the work.

.. index:: gimplify_function_tree, gimplify_expr, lang_hooks.gimplify_expr

The main entry point to this pass is ``gimplify_function_tree``
located in :samp:`gimplify.cc`.  From here we process the entire
function gimplifying each statement in turn.  The main workhorse
for this pass is ``gimplify_expr``.  Approximately everything
passes through here at least once, and it is from here that we
invoke the ``lang_hooks.gimplify_expr`` callback.

The callback should examine the expression in question and return
``GS_UNHANDLED`` if the expression is not a language specific
construct that requires attention.  Otherwise it should alter the
expression in some way to such that forward progress is made toward
producing valid GIMPLE.  If the callback is certain that the
transformation is complete and the expression is valid GIMPLE, it
should return ``GS_ALL_DONE``.  Otherwise it should return
``GS_OK``, which will cause the expression to be processed again.
If the callback encounters an error during the transformation (because
the front end is relying on the gimplification process to finish
semantic checks), it should return ``GS_ERROR``.
