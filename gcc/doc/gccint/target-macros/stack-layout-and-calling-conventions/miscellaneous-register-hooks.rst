..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: miscellaneous register hooks

.. _miscellaneous-register-hooks:

Miscellaneous register hooks
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. c:var:: bool TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS

  .. hook-start:TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS

  Set to true if each call that binds to a local definition explicitly
  clobbers or sets all non-fixed registers modified by performing the call.
  That is, by the call pattern itself, or by code that might be inserted by the
  linker (e.g. stubs, veneers, branch islands), but not including those
  modifiable by the callee.  The affected registers may be mentioned explicitly
  in the call pattern, or included as clobbers in CALL_INSN_FUNCTION_USAGE.
  The default version of this hook is set to false.  The purpose of this hook
  is to enable the fipa-ra optimization.

.. hook-end