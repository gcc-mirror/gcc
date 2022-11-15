..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: annotations

.. _annotations:

Annotations
***********

The optimizers need to associate attributes with variables during the
optimization process.  For instance, we need to know whether a
variable has aliases.  All these attributes are stored in data
structures called annotations which are then linked to the field
``ann`` in ``struct tree_common``.
