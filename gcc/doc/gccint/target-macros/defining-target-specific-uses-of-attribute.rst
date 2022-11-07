..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: target attributes, machine attributes, attributes, target-specific

.. _target-attributes:

Defining target-specific uses of __attribute__
**********************************************

Target-specific attributes may be defined for functions, data and types.
These are described using the following target hooks; they also need to
be documented in :samp:`extend.texi`.

.. include:: tm.rst.in
  :start-after: [TARGET_ATTRIBUTE_TABLE]
  :end-before: [TARGET_ATTRIBUTE_TABLE]


.. include:: tm.rst.in
  :start-after: [TARGET_ATTRIBUTE_TAKES_IDENTIFIER_P]
  :end-before: [TARGET_ATTRIBUTE_TAKES_IDENTIFIER_P]


.. include:: tm.rst.in
  :start-after: [TARGET_COMP_TYPE_ATTRIBUTES]
  :end-before: [TARGET_COMP_TYPE_ATTRIBUTES]


.. include:: tm.rst.in
  :start-after: [TARGET_SET_DEFAULT_TYPE_ATTRIBUTES]
  :end-before: [TARGET_SET_DEFAULT_TYPE_ATTRIBUTES]


.. include:: tm.rst.in
  :start-after: [TARGET_MERGE_TYPE_ATTRIBUTES]
  :end-before: [TARGET_MERGE_TYPE_ATTRIBUTES]


.. include:: tm.rst.in
  :start-after: [TARGET_MERGE_DECL_ATTRIBUTES]
  :end-before: [TARGET_MERGE_DECL_ATTRIBUTES]


.. include:: tm.rst.in
  :start-after: [TARGET_VALID_DLLIMPORT_ATTRIBUTE_P]
  :end-before: [TARGET_VALID_DLLIMPORT_ATTRIBUTE_P]


.. c:macro:: TARGET_DECLSPEC

  Define this macro to a nonzero value if you want to treat
  ``__declspec(X)`` as equivalent to ``__attribute((X))``.  By
  default, this behavior is enabled only for targets that define
  ``TARGET_DLLIMPORT_DECL_ATTRIBUTES``.  The current implementation
  of ``__declspec`` is via a built-in macro, but you should not rely
  on this implementation detail.

.. include:: tm.rst.in
  :start-after: [TARGET_INSERT_ATTRIBUTES]
  :end-before: [TARGET_INSERT_ATTRIBUTES]


.. include:: tm.rst.in
  :start-after: [TARGET_HANDLE_GENERIC_ATTRIBUTE]
  :end-before: [TARGET_HANDLE_GENERIC_ATTRIBUTE]


.. include:: tm.rst.in
  :start-after: [TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P]
  :end-before: [TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P]


.. include:: tm.rst.in
  :start-after: [TARGET_OPTION_VALID_ATTRIBUTE_P]
  :end-before: [TARGET_OPTION_VALID_ATTRIBUTE_P]


.. include:: tm.rst.in
  :start-after: [TARGET_OPTION_SAVE]
  :end-before: [TARGET_OPTION_SAVE]


.. include:: tm.rst.in
  :start-after: [TARGET_OPTION_RESTORE]
  :end-before: [TARGET_OPTION_RESTORE]


.. include:: tm.rst.in
  :start-after: [TARGET_OPTION_POST_STREAM_IN]
  :end-before: [TARGET_OPTION_POST_STREAM_IN]


.. include:: tm.rst.in
  :start-after: [TARGET_OPTION_PRINT]
  :end-before: [TARGET_OPTION_PRINT]


.. include:: tm.rst.in
  :start-after: [TARGET_OPTION_PRAGMA_PARSE]
  :end-before: [TARGET_OPTION_PRAGMA_PARSE]


.. include:: tm.rst.in
  :start-after: [TARGET_OPTION_OVERRIDE]
  :end-before: [TARGET_OPTION_OVERRIDE]


.. include:: tm.rst.in
  :start-after: [TARGET_OPTION_FUNCTION_VERSIONS]
  :end-before: [TARGET_OPTION_FUNCTION_VERSIONS]


.. include:: tm.rst.in
  :start-after: [TARGET_CAN_INLINE_P]
  :end-before: [TARGET_CAN_INLINE_P]


.. include:: tm.rst.in
  :start-after: [TARGET_UPDATE_IPA_FN_TARGET_INFO]
  :end-before: [TARGET_UPDATE_IPA_FN_TARGET_INFO]


.. include:: tm.rst.in
  :start-after: [TARGET_NEED_IPA_FN_TARGET_INFO]
  :end-before: [TARGET_NEED_IPA_FN_TARGET_INFO]


.. include:: tm.rst.in
  :start-after: [TARGET_RELAYOUT_FUNCTION]
  :end-before: [TARGET_RELAYOUT_FUNCTION]
