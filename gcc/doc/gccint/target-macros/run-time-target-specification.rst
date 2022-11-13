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

  .. hook-start:TARGET_DEFAULT_TARGET_FLAGS

  .. hook-end

  This variable specifies the initial value of ``target_flags``.
  Its default setting is 0.

.. index:: optional hardware or system features, features, optional, in system conventions

.. function:: bool TARGET_HANDLE_OPTION (struct gcc_options *opts, struct gcc_options *opts_set, const struct cl_decoded_option *decoded, location_t loc)

  .. hook-start:TARGET_HANDLE_OPTION

  .. hook-end

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

  .. hook-start:TARGET_HANDLE_C_OPTION

  .. hook-end

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

.. function:: tree TARGET_OBJC_CONSTRUCT_STRING_OBJECT (tree string)

  .. hook-start:TARGET_OBJC_CONSTRUCT_STRING_OBJECT

  Targets may provide a string object type that can be used within
  and between C, C++ and their respective Objective-C dialects.
  A string object might, for example, embed encoding and length information.
  These objects are considered opaque to the compiler and handled as references.
  An ideal implementation makes the composition of the string object
  match that of the Objective-C ``NSString`` (``NXString`` for GNUStep),
  allowing efficient interworking between C-only and Objective-C code.
  If a target implements string objects then this hook should return a
  reference to such an object constructed from the normal 'C' string
  representation provided in :samp:`{string}`.
  At present, the hook is used by Objective-C only, to obtain a
  common-format string object when the target provides one.

.. hook-end

.. function:: void TARGET_OBJC_DECLARE_UNRESOLVED_CLASS_REFERENCE (const char *classname)

  .. hook-start:TARGET_OBJC_DECLARE_UNRESOLVED_CLASS_REFERENCE

  Declare that Objective C class :samp:`{classname}` is referenced
  by the current TU.

.. hook-end

.. function:: void TARGET_OBJC_DECLARE_CLASS_DEFINITION (const char *classname)

  .. hook-start:TARGET_OBJC_DECLARE_CLASS_DEFINITION

  Declare that Objective C class :samp:`{classname}` is defined
  by the current TU.

.. hook-end

.. function:: bool TARGET_STRING_OBJECT_REF_TYPE_P (const_tree stringref)

  .. hook-start:TARGET_STRING_OBJECT_REF_TYPE_P

  If a target implements string objects then this hook should return
  ``true`` if :samp:`{stringref}` is a valid reference to such an object.

.. hook-end

.. function:: void TARGET_CHECK_STRING_OBJECT_FORMAT_ARG (tree format_arg, tree args_list)

  .. hook-start:TARGET_CHECK_STRING_OBJECT_FORMAT_ARG

  If a target implements string objects then this hook should
  provide a facility to check the function arguments in :samp:`{args_list}`
  against the format specifiers in :samp:`{format_arg}` where the type of
  :samp:`{format_arg}` is one recognized as a valid string reference type.

.. hook-end

.. function:: void TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE (void)

  .. hook-start:TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE

  This target function is similar to the hook ``TARGET_OPTION_OVERRIDE``
  but is called when the optimize level is changed via an attribute or
  pragma or when it is reset at the end of the code affected by the
  attribute or pragma.  It is not called at the beginning of compilation
  when ``TARGET_OPTION_OVERRIDE`` is called so if you want to perform these
  actions then, you should have ``TARGET_OPTION_OVERRIDE`` call
  ``TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE``.

.. hook-end

.. c:macro:: C_COMMON_OVERRIDE_OPTIONS

  This is similar to the ``TARGET_OPTION_OVERRIDE`` hook
  but is only used in the C
  language frontends (C, Objective-C, C++, Objective-C++) and so can be
  used to alter option flag variables which only exist in those
  frontends.

.. c:var:: const struct default_options * TARGET_OPTION_OPTIMIZATION_TABLE

  .. hook-start:TARGET_OPTION_OPTIMIZATION_TABLE

  .. hook-end

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

.. function:: void TARGET_OPTION_INIT_STRUCT (struct gcc_options *opts)

  .. hook-start:TARGET_OPTION_INIT_STRUCT

  Set target-dependent initial values of fields in :samp:`{opts}`.

.. hook-end

.. function:: const char * TARGET_COMPUTE_MULTILIB (const struct switchstr *switches, int n_switches, const char *multilib_dir, const char *multilib_defaults, const char *multilib_select, const char *multilib_matches, const char *multilib_exclusions, const char *multilib_reuse)

  .. hook-start:TARGET_COMPUTE_MULTILIB

  Some targets like RISC-V might have complicated multilib reuse rules which
  are hard to implement with the current multilib scheme.  This hook allows
  targets to override the result from the built-in multilib mechanism.
  :samp:`{switches}` is the raw option list with :samp:`{n_switches}` items;
  :samp:`{multilib_dir}` is the multi-lib result which is computed by the built-in
  multi-lib mechanism;
  :samp:`{multilib_defaults}` is the default options list for multi-lib;
  :samp:`{multilib_select}` is the string containing the list of supported
  multi-libs, and the option checking list.
  :samp:`{multilib_matches}`, :samp:`{multilib_exclusions}`, and :samp:`{multilib_reuse}`
  are corresponding to :samp:`{MULTILIB_MATCHES}`, :samp:`{MULTILIB_EXCLUSIONS}`,
  and :samp:`{MULTILIB_REUSE}`.
  The default definition does nothing but return :samp:`{multilib_dir}` directly.

.. hook-end

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

.. function:: bool TARGET_FLOAT_EXCEPTIONS_ROUNDING_SUPPORTED_P (void)

  .. hook-start:TARGET_FLOAT_EXCEPTIONS_ROUNDING_SUPPORTED_P

  Returns true if the target supports IEEE 754 floating-point exceptions
  and rounding modes, false otherwise.  This is intended to relate to the
  ``float`` and ``double`` types, but not necessarily ``long double``.
  By default, returns true if the ``adddf3`` instruction pattern is
  available and false otherwise, on the assumption that hardware floating
  point supports exceptions and rounding modes but software floating point
  does not.

.. hook-end
