..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: library subroutine names, libgcc.a

.. _library-calls:

Implicit Calls to Library Routines
**********************************

.. prevent bad page break with this line

Here is an explanation of implicit calls to library routines.

.. c:macro:: DECLARE_LIBRARY_RENAMES

  This macro, if defined, should expand to a piece of C code that will get
  expanded when compiling functions for libgcc.a.  It can be used to
  provide alternate names for GCC's internal library functions if there
  are ABI-mandated names that the compiler should provide.

.. index:: set_optab_libfunc, init_one_libfunc

.. include:: tm.rst.in
  :start-after: [TARGET_INIT_LIBFUNCS]
  :end-before: [TARGET_INIT_LIBFUNCS]


.. include:: tm.rst.in
  :start-after: [TARGET_LIBFUNC_GNU_PREFIX]
  :end-before: [TARGET_LIBFUNC_GNU_PREFIX]


.. c:macro:: FLOAT_LIB_COMPARE_RETURNS_BOOL (mode, comparison)

  This macro should return ``true`` if the library routine that
  implements the floating point comparison operator :samp:`{comparison}` in
  mode :samp:`{mode}` will return a boolean, and :samp:`{false}` if it will
  return a tristate.

  GCC's own floating point libraries return tristates from the
  comparison operators, so the default returns false always.  Most ports
  don't need to define this macro.

.. c:macro:: TARGET_LIB_INT_CMP_BIASED

  This macro should evaluate to ``true`` if the integer comparison
  functions (like ``__cmpdi2``) return 0 to indicate that the first
  operand is smaller than the second, 1 to indicate that they are equal,
  and 2 to indicate that the first operand is greater than the second.
  If this macro evaluates to ``false`` the comparison functions return
  -1, 0, and 1 instead of 0, 1, and 2.  If the target uses the routines
  in :samp:`libgcc.a`, you do not need to define this macro.

.. c:macro:: TARGET_HAS_NO_HW_DIVIDE

  This macro should be defined if the target has no hardware divide
  instructions.  If this macro is defined, GCC will use an algorithm which
  make use of simple logical and arithmetic operations for 64-bit
  division.  If the macro is not defined, GCC will use an algorithm which
  make use of a 64-bit by 32-bit divide primitive.

.. index:: EDOM, implicit usage, matherr

.. c:macro:: TARGET_EDOM

  The value of ``EDOM`` on the target machine, as a C integer constant
  expression.  If you don't define this macro, GCC does not attempt to
  deposit the value of ``EDOM`` into ``errno`` directly.  Look in
  :samp:`/usr/include/errno.h` to find the value of ``EDOM`` on your
  system.

  If you do not define ``TARGET_EDOM``, then compiled code reports
  domain errors by calling the library function and letting it report the
  error.  If mathematical functions on your system use ``matherr`` when
  there is an error, then you should leave ``TARGET_EDOM`` undefined so
  that ``matherr`` is used normally.

.. index:: errno, implicit usage

.. c:macro:: GEN_ERRNO_RTX

  Define this macro as a C expression to create an rtl expression that
  refers to the global 'variable' ``errno``.  (On certain systems,
  ``errno`` may not actually be a variable.)  If you don't define this
  macro, a reasonable default is used.

.. include:: tm.rst.in
  :start-after: [TARGET_LIBC_HAS_FUNCTION]
  :end-before: [TARGET_LIBC_HAS_FUNCTION]


.. include:: tm.rst.in
  :start-after: [TARGET_LIBC_HAS_FAST_FUNCTION]
  :end-before: [TARGET_LIBC_HAS_FAST_FUNCTION]


.. c:macro:: NEXT_OBJC_RUNTIME

  Set this macro to 1 to use the "NeXT" Objective-C message sending conventions
  by default.  This calling convention involves passing the object, the selector
  and the method arguments all at once to the method-lookup library function.
  This is the usual setting when targeting Darwin/Mac OS X systems, which have
  the NeXT runtime installed.

  If the macro is set to 0, the "GNU" Objective-C message sending convention
  will be used by default.  This convention passes just the object and the
  selector to the method-lookup function, which returns a pointer to the method.

  In either case, it remains possible to select code-generation for the alternate
  scheme, by means of compiler command line switches.
