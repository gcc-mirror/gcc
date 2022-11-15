..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Emulated TLS

.. _emulated-tls:

Emulating TLS
*************

For targets whose psABI does not provide Thread Local Storage via
specific relocations and instruction sequences, an emulation layer is
used.  A set of target hooks allows this emulation layer to be
configured for the requirements of a particular target.  For instance
the psABI may in fact specify TLS support in terms of an emulation
layer.

The emulation layer works by creating a control object for every TLS
object.  To access the TLS object, a lookup function is provided
which, when given the address of the control object, will return the
address of the current thread's instance of the TLS object.

.. include:: tm.rst.in
  :start-after: [TARGET_EMUTLS_GET_ADDRESS]
  :end-before: [TARGET_EMUTLS_GET_ADDRESS]


.. include:: tm.rst.in
  :start-after: [TARGET_EMUTLS_REGISTER_COMMON]
  :end-before: [TARGET_EMUTLS_REGISTER_COMMON]


.. include:: tm.rst.in
  :start-after: [TARGET_EMUTLS_VAR_SECTION]
  :end-before: [TARGET_EMUTLS_VAR_SECTION]


.. include:: tm.rst.in
  :start-after: [TARGET_EMUTLS_TMPL_SECTION]
  :end-before: [TARGET_EMUTLS_TMPL_SECTION]


.. include:: tm.rst.in
  :start-after: [TARGET_EMUTLS_VAR_PREFIX]
  :end-before: [TARGET_EMUTLS_VAR_PREFIX]


.. include:: tm.rst.in
  :start-after: [TARGET_EMUTLS_TMPL_PREFIX]
  :end-before: [TARGET_EMUTLS_TMPL_PREFIX]


.. include:: tm.rst.in
  :start-after: [TARGET_EMUTLS_VAR_FIELDS]
  :end-before: [TARGET_EMUTLS_VAR_FIELDS]


.. include:: tm.rst.in
  :start-after: [TARGET_EMUTLS_VAR_INIT]
  :end-before: [TARGET_EMUTLS_VAR_INIT]


.. include:: tm.rst.in
  :start-after: [TARGET_EMUTLS_VAR_ALIGN_FIXED]
  :end-before: [TARGET_EMUTLS_VAR_ALIGN_FIXED]


.. include:: tm.rst.in
  :start-after: [TARGET_EMUTLS_DEBUG_FORM_TLS_ADDRESS]
  :end-before: [TARGET_EMUTLS_DEBUG_FORM_TLS_ADDRESS]
