..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: parameters, c++ abi

.. _c++-abi:

C++ ABI parameters
******************

.. function:: tree TARGET_CXX_GUARD_TYPE (void)

  .. hook-start:TARGET_CXX_GUARD_TYPE

  Define this hook to override the integer type used for guard variables.
  These are used to implement one-time construction of static objects.  The
  default is long_long_integer_type_node.

.. hook-end

.. function:: bool TARGET_CXX_GUARD_MASK_BIT (void)

  .. hook-start:TARGET_CXX_GUARD_MASK_BIT

  This hook determines how guard variables are used.  It should return
  ``false`` (the default) if the first byte should be used.  A return value of
  ``true`` indicates that only the least significant bit should be used.

.. hook-end

.. function:: tree TARGET_CXX_GET_COOKIE_SIZE (tree type)

  .. hook-start:TARGET_CXX_GET_COOKIE_SIZE

  This hook returns the size of the cookie to use when allocating an array
  whose elements have the indicated :samp:`{type}`.  Assumes that it is already
  known that a cookie is needed.  The default is
  ``max(sizeof (size_t), alignof(type))``, as defined in section 2.7 of the
  IA64/Generic C++ ABI.

.. hook-end

.. function:: bool TARGET_CXX_COOKIE_HAS_SIZE (void)

  .. hook-start:TARGET_CXX_COOKIE_HAS_SIZE

  This hook should return ``true`` if the element size should be stored in
  array cookies.  The default is to return ``false``.

.. hook-end

.. function:: int TARGET_CXX_IMPORT_EXPORT_CLASS (tree type, int import_export)

  .. hook-start:TARGET_CXX_IMPORT_EXPORT_CLASS

  If defined by a backend this hook allows the decision made to export
  class :samp:`{type}` to be overruled.  Upon entry :samp:`{import_export}`
  will contain 1 if the class is going to be exported, -1 if it is going
  to be imported and 0 otherwise.  This function should return the
  modified value and perform any other actions necessary to support the
  backend's targeted operating system.

.. hook-end

.. function:: bool TARGET_CXX_CDTOR_RETURNS_THIS (void)

  .. hook-start:TARGET_CXX_CDTOR_RETURNS_THIS

  This hook should return ``true`` if constructors and destructors return
  the address of the object created/destroyed.  The default is to return
  ``false``.

.. hook-end

.. function:: bool TARGET_CXX_KEY_METHOD_MAY_BE_INLINE (void)

  .. hook-start:TARGET_CXX_KEY_METHOD_MAY_BE_INLINE

  This hook returns true if the key method for a class (i.e., the method
  which, if defined in the current translation unit, causes the virtual
  table to be emitted) may be an inline function.  Under the standard
  Itanium C++ ABI the key method may be an inline function so long as
  the function is not declared inline in the class definition.  Under
  some variants of the ABI, an inline function can never be the key
  method.  The default is to return ``true``.

.. hook-end

.. function:: void TARGET_CXX_DETERMINE_CLASS_DATA_VISIBILITY (tree decl)

  .. hook-start:TARGET_CXX_DETERMINE_CLASS_DATA_VISIBILITY

  :samp:`{decl}` is a virtual table, virtual table table, typeinfo object,
  or other similar implicit class data object that will be emitted with
  external linkage in this translation unit.  No ELF visibility has been
  explicitly specified.  If the target needs to specify a visibility
  other than that of the containing class, use this hook to set
  ``DECL_VISIBILITY`` and ``DECL_VISIBILITY_SPECIFIED``.

.. hook-end

.. function:: bool TARGET_CXX_CLASS_DATA_ALWAYS_COMDAT (void)

  .. hook-start:TARGET_CXX_CLASS_DATA_ALWAYS_COMDAT

  This hook returns true (the default) if virtual tables and other
  similar implicit class data objects are always COMDAT if they have
  external linkage.  If this hook returns false, then class data for
  classes whose virtual table will be emitted in only one translation
  unit will not be COMDAT.

.. hook-end

.. function:: bool TARGET_CXX_LIBRARY_RTTI_COMDAT (void)

  .. hook-start:TARGET_CXX_LIBRARY_RTTI_COMDAT

  This hook returns true (the default) if the RTTI information for
  the basic types which is defined in the C++ runtime should always
  be COMDAT, false if it should not be COMDAT.

.. hook-end

.. function:: bool TARGET_CXX_USE_AEABI_ATEXIT (void)

  .. hook-start:TARGET_CXX_USE_AEABI_ATEXIT

  This hook returns true if ``__aeabi_atexit`` (as defined by the ARM EABI)
  should be used to register static destructors when :option:`-fuse-cxa-atexit`
  is in effect.  The default is to return false to use ``__cxa_atexit``.

.. hook-end

.. function:: bool TARGET_CXX_USE_ATEXIT_FOR_CXA_ATEXIT (void)

  .. hook-start:TARGET_CXX_USE_ATEXIT_FOR_CXA_ATEXIT

  This hook returns true if the target ``atexit`` function can be used
  in the same manner as ``__cxa_atexit`` to register C++ static
  destructors. This requires that ``atexit`` -registered functions in
  shared libraries are run in the correct order when the libraries are
  unloaded. The default is to return false.

.. hook-end

.. function:: void TARGET_CXX_ADJUST_CLASS_AT_DEFINITION (tree type)

  .. hook-start:TARGET_CXX_ADJUST_CLASS_AT_DEFINITION

  :samp:`{type}` is a C++ class (i.e., RECORD_TYPE or UNION_TYPE) that has just
  been defined.  Use this hook to make adjustments to the class (eg, tweak
  visibility or perform any other required target modifications).

.. hook-end

.. function:: tree TARGET_CXX_DECL_MANGLING_CONTEXT (const_tree decl)

  .. hook-start:TARGET_CXX_DECL_MANGLING_CONTEXT

  Return target-specific mangling context of :samp:`{decl}` or ``NULL_TREE``.

.. hook-end