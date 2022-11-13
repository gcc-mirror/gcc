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

.. function:: sbitmap TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS (void)

  .. hook-start:TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS

  This hook should return an ``sbitmap`` with the bits set for those
  components that can be separately shrink-wrapped in the current function.
  Return ``NULL`` if the current function should not get any separate
  shrink-wrapping.
  Don't define this hook if it would always return ``NULL``.
  If it is defined, the other hooks in this group have to be defined as well.

.. hook-end

.. function:: sbitmap TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB (basic_block)

  .. hook-start:TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB

  This hook should return an ``sbitmap`` with the bits set for those
  components where either the prologue component has to be executed before
  the ``basic_block``, or the epilogue component after it, or both.

.. hook-end

.. function:: void TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS (sbitmap components, edge e, sbitmap edge_components, bool is_prologue)

  .. hook-start:TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS

  This hook should clear the bits in the :samp:`{components}` bitmap for those
  components in :samp:`{edge_components}` that the target cannot handle on edge
  :samp:`{e}`, where :samp:`{is_prologue}` says if this is for a prologue or an
  epilogue instead.

.. hook-end

.. function:: void TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS (sbitmap)

  .. hook-start:TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS

  Emit prologue insns for the components indicated by the parameter.

.. hook-end

.. function:: void TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS (sbitmap)

  .. hook-start:TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS

  Emit epilogue insns for the components indicated by the parameter.

.. hook-end

.. function:: void TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS (sbitmap)

  .. hook-start:TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS

  Mark the components in the parameter as handled, so that the
  ``prologue`` and ``epilogue`` named patterns know to ignore those
  components.  The target code should not hang on to the ``sbitmap``, it
  will be deleted after this call.

.. hook-end