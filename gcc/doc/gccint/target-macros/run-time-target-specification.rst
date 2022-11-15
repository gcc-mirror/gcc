..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: run-time target specification, predefined macros, target specifications

.. _run-time-target:

Run-time Target Specification
*****************************

.. prevent bad page break with this line

Here are run-time target specifications.

.. c:macro:: TARGET_CPU_CPP_BUILTINS ()

  This function-like macro expands to a block of code that defines
  built-in preprocessor macros and assertions for the target CPU, using
  the functions ``builtin_define``, ``builtin_define_std`` and
  ``builtin_assert``.  When the front end
  calls this macro it provides a trailing semicolon, and since it has
  finished command line option processing your code can use those
  results freely.

  ``builtin_assert`` takes a string in the form you pass to the
  command-line option :option:`-A`, such as ``cpu=mips``, and creates
  the assertion.  ``builtin_define`` takes a string in the form
  accepted by option :option:`-D` and unconditionally defines the macro.

  ``builtin_define_std`` takes a string representing the name of an
  object-like macro.  If it doesn't lie in the user's namespace,
  ``builtin_define_std`` defines it unconditionally.  Otherwise, it
  defines a version with two leading underscores, and another version
  with two leading and trailing underscores, and defines the original
  only if an ISO standard was not requested on the command line.  For
  example, passing ``unix`` defines ``__unix``, ``__unix__``
  and possibly ``unix`` ; passing ``_mips`` defines ``__mips``,
  ``__mips__`` and possibly ``_mips``, and passing ``_ABI64``
  defines only ``_ABI64``.

  You can also test for the C dialect being compiled.  The variable
  ``c_language`` is set to one of ``clk_c``, ``clk_cplusplus``
  or ``clk_objective_c``.  Note that if we are preprocessing
  assembler, this variable will be ``clk_c`` but the function-like
  macro ``preprocessing_asm_p()`` will return true, so you might want
  to check for that first.  If you need to check for strict ANSI, the
  variable ``flag_iso`` can be used.  The function-like macro
  ``preprocessing_trad_p()`` can be used to check for traditional
  preprocessing.

.. c:macro:: TARGET_OS_CPP_BUILTINS ()

  Similarly to ``TARGET_CPU_CPP_BUILTINS`` but this macro is optional
  and is used for the target operating system instead.

.. c:macro:: TARGET_OBJFMT_CPP_BUILTINS ()

  Similarly to ``TARGET_CPU_CPP_BUILTINS`` but this macro is optional
  and is used for the target object format.  :samp:`elfos.h` uses this
  macro to define ``__ELF__``, so you probably do not need to define
  it yourself.

.. index:: target_flags

Variable extern int target_flagsThis variable is declared in :samp:`options.h`, which is included before
any target-specific headers.

.. c:var:: int TARGET_DEFAULT_TARGET_FLAGS

  This variable specifies the initial value of ``target_flags``.
  Its default setting is 0.

.. index:: optional hardware or system features, features, optional, in system conventions


.. function:: bool TARGET_HANDLE_OPTION (struct gcc_options *opts, struct gcc_options *opts_set, const struct cl_decoded_option *decoded, location_t loc)

  This hook is called whenever the user specifies one of the
  target-specific options described by the :samp:`.opt` definition files
  (see :ref:`options`).  It has the opportunity to do some option-specific
  processing and should return true if the option is valid.  The default
  definition does nothing but return true.

  :samp:`{decoded}` specifies the option and its arguments.  :samp:`{opts}` and
  :samp:`{opts_set}` are the ``gcc_options`` structures to be used for
  storing option state, and :samp:`{loc}` is the location at which the
  option was passed (``UNKNOWN_LOCATION`` except for options passed
  via attributes).

.. function:: bool TARGET_HANDLE_C_OPTION (size_t code, const char *arg, int value)

  This target hook is called whenever the user specifies one of the
  target-specific C language family options described by the :samp:`.opt`
  definition files(see :ref:`options`).  It has the opportunity to do some
  option-specific processing and should return true if the option is
  valid.  The arguments are like for ``TARGET_HANDLE_OPTION``.  The
  default definition does nothing but return false.

  In general, you should use ``TARGET_HANDLE_OPTION`` to handle
  options.  However, if processing an option requires routines that are
  only available in the C (and related language) front ends, then you
  should use ``TARGET_HANDLE_C_OPTION`` instead.

.. include:: tm.rst.in
  :start-after: [TARGET_OBJC_CONSTRUCT_STRING_OBJECT]
  :end-before: [TARGET_OBJC_CONSTRUCT_STRING_OBJECT]


.. include:: tm.rst.in
  :start-after: [TARGET_OBJC_DECLARE_UNRESOLVED_CLASS_REFERENCE]
  :end-before: [TARGET_OBJC_DECLARE_UNRESOLVED_CLASS_REFERENCE]


.. include:: tm.rst.in
  :start-after: [TARGET_OBJC_DECLARE_CLASS_DEFINITION]
  :end-before: [TARGET_OBJC_DECLARE_CLASS_DEFINITION]


.. include:: tm.rst.in
  :start-after: [TARGET_STRING_OBJECT_REF_TYPE_P]
  :end-before: [TARGET_STRING_OBJECT_REF_TYPE_P]


.. include:: tm.rst.in
  :start-after: [TARGET_CHECK_STRING_OBJECT_FORMAT_ARG]
  :end-before: [TARGET_CHECK_STRING_OBJECT_FORMAT_ARG]


.. include:: tm.rst.in
  :start-after: [TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE]
  :end-before: [TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE]


.. c:macro:: C_COMMON_OVERRIDE_OPTIONS

  This is similar to the ``TARGET_OPTION_OVERRIDE`` hook
  but is only used in the C
  language frontends (C, Objective-C, C++, Objective-C++) and so can be
  used to alter option flag variables which only exist in those
  frontends.

.. c:var:: const struct default_options * TARGET_OPTION_OPTIMIZATION_TABLE

  Some machines may desire to change what optimizations are performed for
  various optimization levels.   This variable, if defined, describes
  options to enable at particular sets of optimization levels.  These
  options are processed once
  just after the optimization level is determined and before the remainder
  of the command options have been parsed, so may be overridden by other
  options passed explicitly.

  This processing is run once at program startup and when the optimization
  options are changed via ``#pragma GCC optimize`` or by using the
  ``optimize`` attribute.

.. include:: tm.rst.in
  :start-after: [TARGET_OPTION_INIT_STRUCT]
  :end-before: [TARGET_OPTION_INIT_STRUCT]


.. include:: tm.rst.in
  :start-after: [TARGET_COMPUTE_MULTILIB]
  :end-before: [TARGET_COMPUTE_MULTILIB]


.. c:macro:: SWITCHABLE_TARGET

  Some targets need to switch between substantially different subtargets
  during compilation.  For example, the MIPS target has one subtarget for
  the traditional MIPS architecture and another for MIPS16.  Source code
  can switch between these two subarchitectures using the ``mips16``
  and ``nomips16`` attributes.

  Such subtargets can differ in things like the set of available
  registers, the set of available instructions, the costs of various
  operations, and so on.  GCC caches a lot of this type of information
  in global variables, and recomputing them for each subtarget takes a
  significant amount of time.  The compiler therefore provides a facility
  for maintaining several versions of the global variables and quickly
  switching between them; see :samp:`target-globals.h` for details.

  Define this macro to 1 if your target needs this facility.  The default
  is 0.

.. include:: tm.rst.in
  :start-after: [TARGET_FLOAT_EXCEPTIONS_ROUNDING_SUPPORTED_P]
  :end-before: [TARGET_FLOAT_EXCEPTIONS_ROUNDING_SUPPORTED_P]
