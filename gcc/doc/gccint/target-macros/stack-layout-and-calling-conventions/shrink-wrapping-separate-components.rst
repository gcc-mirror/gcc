..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: shrink-wrapping separate components

.. _shrink-wrapping-separate-components:

Shrink-wrapping separate components
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The prologue may perform a variety of target dependent tasks such as
saving callee-saved registers, saving the return address, aligning the
stack, creating a stack frame, initializing the PIC register, setting
up the static chain, etc.

On some targets some of these tasks may be independent of others and
thus may be shrink-wrapped separately.  These independent tasks are
referred to as components and are handled generically by the target
independent parts of GCC.

Using the following hooks those prologue or epilogue components can be
shrink-wrapped separately, so that the initialization (and possibly
teardown) those components do is not done as frequently on execution
paths where this would unnecessary.

What exactly those components are is up to the target code; the generic
code treats them abstractly, as a bit in an ``sbitmap``.  These
``sbitmap`` s are allocated by the ``shrink_wrap.get_separate_components``
and ``shrink_wrap.components_for_bb`` hooks, and deallocated by the
generic code.

.. include:: ../tm.rst.in
  :start-after: [TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS]
  :end-before: [TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS]


.. include:: ../tm.rst.in
  :start-after: [TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB]
  :end-before: [TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB]


.. include:: ../tm.rst.in
  :start-after: [TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS]
  :end-before: [TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS]


.. include:: ../tm.rst.in
  :start-after: [TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS]
  :end-before: [TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS]


.. include:: ../tm.rst.in
  :start-after: [TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS]
  :end-before: [TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS]


.. include:: ../tm.rst.in
  :start-after: [TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS]
  :end-before: [TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS]
