..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: driver, controlling the compilation driver

.. _driver:

Controlling the Compilation Driver, gcc
***************************************

.. prevent bad page break with this line

You can control the compilation driver.

.. c:macro:: DRIVER_SELF_SPECS

  A list of specs for the driver itself.  It should be a suitable
  initializer for an array of strings, with no surrounding braces.

  The driver applies these specs to its own command line between loading
  default :samp:`specs` files (but not command-line specified ones) and
  choosing the multilib directory or running any subcommands.  It
  applies them in the order given, so each spec can depend on the
  options added by earlier ones.  It is also possible to remove options
  using :samp:`%<{option}` in the usual way.

  This macro can be useful when a port has several interdependent target
  options.  It provides a way of standardizing the command line so
  that the other specs are easier to write.

  Do not define this macro if it does not need to do anything.

.. c:macro:: OPTION_DEFAULT_SPECS

  A list of specs used to support configure-time default options (i.e.
  :option:`--with` options) in the driver.  It should be a suitable initializer
  for an array of structures, each containing two strings, without the
  outermost pair of surrounding braces.

  The first item in the pair is the name of the default.  This must match
  the code in :samp:`config.gcc` for the target.  The second item is a spec
  to apply if a default with this name was specified.  The string
  :samp:`%(VALUE)` in the spec will be replaced by the value of the default
  everywhere it occurs.

  The driver will apply these specs to its own command line between loading
  default :samp:`specs` files and processing ``DRIVER_SELF_SPECS``, using
  the same mechanism as ``DRIVER_SELF_SPECS``.

  Do not define this macro if it does not need to do anything.

.. c:macro:: CPP_SPEC

  A C string constant that tells the GCC driver program options to
  pass to CPP.  It can also specify how to translate options you
  give to GCC into options for GCC to pass to the CPP.

  Do not define this macro if it does not need to do anything.

.. c:macro:: CPLUSPLUS_CPP_SPEC

  This macro is just like ``CPP_SPEC``, but is used for C++, rather
  than C.  If you do not define this macro, then the value of
  ``CPP_SPEC`` (if any) will be used instead.

.. c:macro:: CC1_SPEC

  A C string constant that tells the GCC driver program options to
  pass to ``cc1``, ``cc1plus``, ``f771``, and the other language
  front ends.
  It can also specify how to translate options you give to GCC into options
  for GCC to pass to front ends.

  Do not define this macro if it does not need to do anything.

.. c:macro:: CC1PLUS_SPEC

  A C string constant that tells the GCC driver program options to
  pass to ``cc1plus``.  It can also specify how to translate options you
  give to GCC into options for GCC to pass to the ``cc1plus``.

  Do not define this macro if it does not need to do anything.
  Note that everything defined in CC1_SPEC is already passed to
  ``cc1plus`` so there is no need to duplicate the contents of
  CC1_SPEC in CC1PLUS_SPEC.

.. c:macro:: ASM_SPEC

  A C string constant that tells the GCC driver program options to
  pass to the assembler.  It can also specify how to translate options
  you give to GCC into options for GCC to pass to the assembler.
  See the file :samp:`sun3.h` for an example of this.

  Do not define this macro if it does not need to do anything.

.. c:macro:: ASM_FINAL_SPEC

  A C string constant that tells the GCC driver program how to
  run any programs which cleanup after the normal assembler.
  Normally, this is not needed.  See the file :samp:`mips.h` for
  an example of this.

  Do not define this macro if it does not need to do anything.

.. c:macro:: AS_NEEDS_DASH_FOR_PIPED_INPUT

  Define this macro, with no value, if the driver should give the assembler
  an argument consisting of a single dash, :option:`-`, to instruct it to
  read from its standard input (which will be a pipe connected to the
  output of the compiler proper).  This argument is given after any
  :option:`-o` option specifying the name of the output file.

  If you do not define this macro, the assembler is assumed to read its
  standard input if given no non-option arguments.  If your assembler
  cannot read standard input at all, use a :samp:`%{pipe:%e}` construct;
  see :samp:`mips.h` for instance.

.. c:macro:: LINK_SPEC

  A C string constant that tells the GCC driver program options to
  pass to the linker.  It can also specify how to translate options you
  give to GCC into options for GCC to pass to the linker.

  Do not define this macro if it does not need to do anything.

.. c:macro:: LIB_SPEC

  Another C string constant used much like ``LINK_SPEC``.  The difference
  between the two is that ``LIB_SPEC`` is used at the end of the
  command given to the linker.

  If this macro is not defined, a default is provided that
  loads the standard C library from the usual place.  See :samp:`gcc.cc`.

.. c:macro:: LIBGCC_SPEC

  Another C string constant that tells the GCC driver program
  how and when to place a reference to :samp:`libgcc.a` into the
  linker command line.  This constant is placed both before and after
  the value of ``LIB_SPEC``.

  If this macro is not defined, the GCC driver provides a default that
  passes the string :option:`-lgcc` to the linker.

.. c:macro:: REAL_LIBGCC_SPEC

  By default, if ``ENABLE_SHARED_LIBGCC`` is defined, the
  ``LIBGCC_SPEC`` is not directly used by the driver program but is
  instead modified to refer to different versions of :samp:`libgcc.a`
  depending on the values of the command line flags :option:`-static`,
  :option:`-shared`, :option:`-static-libgcc`, and :option:`-shared-libgcc`.  On
  targets where these modifications are inappropriate, define
  ``REAL_LIBGCC_SPEC`` instead.  ``REAL_LIBGCC_SPEC`` tells the
  driver how to place a reference to :samp:`libgcc` on the link command
  line, but, unlike ``LIBGCC_SPEC``, it is used unmodified.

.. c:macro:: USE_LD_AS_NEEDED

  A macro that controls the modifications to ``LIBGCC_SPEC``
  mentioned in ``REAL_LIBGCC_SPEC``.  If nonzero, a spec will be
  generated that uses :option:`--as-needed` or equivalent options and the
  shared :samp:`libgcc` in place of the
  static exception handler library, when linking without any of
  ``-static``, ``-static-libgcc``, or ``-shared-libgcc``.

.. c:macro:: LINK_EH_SPEC

  If defined, this C string constant is added to ``LINK_SPEC``.
  When ``USE_LD_AS_NEEDED`` is zero or undefined, it also affects
  the modifications to ``LIBGCC_SPEC`` mentioned in
  ``REAL_LIBGCC_SPEC``.

.. c:macro:: STARTFILE_SPEC

  Another C string constant used much like ``LINK_SPEC``.  The
  difference between the two is that ``STARTFILE_SPEC`` is used at
  the very beginning of the command given to the linker.

  If this macro is not defined, a default is provided that loads the
  standard C startup file from the usual place.  See :samp:`gcc.cc`.

.. c:macro:: ENDFILE_SPEC

  Another C string constant used much like ``LINK_SPEC``.  The
  difference between the two is that ``ENDFILE_SPEC`` is used at
  the very end of the command given to the linker.

  Do not define this macro if it does not need to do anything.

.. c:macro:: THREAD_MODEL_SPEC

  GCC ``-v`` will print the thread model GCC was configured to use.
  However, this doesn't work on platforms that are multilibbed on thread
  models, such as AIX 4.3.  On such platforms, define
  ``THREAD_MODEL_SPEC`` such that it evaluates to a string without
  blanks that names one of the recognized thread models.  ``%*``, the
  default value of this macro, will expand to the value of
  ``thread_file`` set in :samp:`config.gcc`.

.. c:macro:: SYSROOT_SUFFIX_SPEC

  Define this macro to add a suffix to the target sysroot when GCC is
  configured with a sysroot.  This will cause GCC to search for usr/lib,
  et al, within sysroot+suffix.

.. c:macro:: SYSROOT_HEADERS_SUFFIX_SPEC

  Define this macro to add a headers_suffix to the target sysroot when
  GCC is configured with a sysroot.  This will cause GCC to pass the
  updated sysroot+headers_suffix to CPP, causing it to search for
  usr/include, et al, within sysroot+headers_suffix.

.. c:macro:: EXTRA_SPECS

  Define this macro to provide additional specifications to put in the
  :samp:`specs` file that can be used in various specifications like
  ``CC1_SPEC``.

  The definition should be an initializer for an array of structures,
  containing a string constant, that defines the specification name, and a
  string constant that provides the specification.

  Do not define this macro if it does not need to do anything.

  ``EXTRA_SPECS`` is useful when an architecture contains several
  related targets, which have various ``..._SPECS`` which are similar
  to each other, and the maintainer would like one central place to keep
  these definitions.

  For example, the PowerPC System V.4 targets use ``EXTRA_SPECS`` to
  define either ``_CALL_SYSV`` when the System V calling sequence is
  used or ``_CALL_AIX`` when the older AIX-based calling sequence is
  used.

  The :samp:`config/rs6000/rs6000.h` target file defines:

  .. code-block:: c++

    #define EXTRA_SPECS \
      { "cpp_sysv_default", CPP_SYSV_DEFAULT },

    #define CPP_SYS_DEFAULT ""

  The :samp:`config/rs6000/sysv.h` target file defines:

  .. code-block:: c++

    #undef CPP_SPEC
    #define CPP_SPEC \
    "%{posix: -D_POSIX_SOURCE } \
    %{mcall-sysv: -D_CALL_SYSV } \
    %{!mcall-sysv: %(cpp_sysv_default) } \
    %{msoft-float: -D_SOFT_FLOAT} %{mcpu=403: -D_SOFT_FLOAT}"

    #undef CPP_SYSV_DEFAULT
    #define CPP_SYSV_DEFAULT "-D_CALL_SYSV"

  while the :samp:`config/rs6000/eabiaix.h` target file defines
  ``CPP_SYSV_DEFAULT`` as:

  .. code-block:: c++

    #undef CPP_SYSV_DEFAULT
    #define CPP_SYSV_DEFAULT "-D_CALL_AIX"

.. c:macro:: LINK_LIBGCC_SPECIAL_1

  Define this macro if the driver program should find the library
  :samp:`libgcc.a`.  If you do not define this macro, the driver program will pass
  the argument :option:`-lgcc` to tell the linker to do the search.

.. c:macro:: LINK_GCC_C_SEQUENCE_SPEC

  The sequence in which libgcc and libc are specified to the linker.
  By default this is ``%G %L %G``.

.. c:macro:: POST_LINK_SPEC

  Define this macro to add additional steps to be executed after linker.
  The default value of this macro is empty string.

.. c:macro:: LINK_COMMAND_SPEC

  A C string constant giving the complete command line need to execute the
  linker.  When you do this, you will need to update your port each time a
  change is made to the link command line within :samp:`gcc.cc`.  Therefore,
  define this macro only if you need to completely redefine the command
  line for invoking the linker and there is no other way to accomplish
  the effect you need.  Overriding this macro may be avoidable by overriding
  ``LINK_GCC_C_SEQUENCE_SPEC`` instead.

.. include:: tm.rst.in
  :start-after: [TARGET_ALWAYS_STRIP_DOTDOT]
  :end-before: [TARGET_ALWAYS_STRIP_DOTDOT]


.. c:macro:: MULTILIB_DEFAULTS

  Define this macro as a C expression for the initializer of an array of
  string to tell the driver program which options are defaults for this
  target and thus do not need to be handled specially when using
  ``MULTILIB_OPTIONS``.

  Do not define this macro if ``MULTILIB_OPTIONS`` is not defined in
  the target makefile fragment or if none of the options listed in
  ``MULTILIB_OPTIONS`` are set by default.
  See :ref:`target-fragment`.

.. c:macro:: RELATIVE_PREFIX_NOT_LINKDIR

  Define this macro to tell :command:`gcc` that it should only translate
  a :option:`-B` prefix into a :option:`-L` linker option if the prefix
  indicates an absolute file name.

.. c:macro:: MD_EXEC_PREFIX

  If defined, this macro is an additional prefix to try after
  ``STANDARD_EXEC_PREFIX``.  ``MD_EXEC_PREFIX`` is not searched
  when the compiler is built as a cross
  compiler.  If you define ``MD_EXEC_PREFIX``, then be sure to add it
  to the list of directories used to find the assembler in :samp:`configure.ac`.

.. c:macro:: STANDARD_STARTFILE_PREFIX

  Define this macro as a C string constant if you wish to override the
  standard choice of ``libdir`` as the default prefix to
  try when searching for startup files such as :samp:`crt0.o`.
  ``STANDARD_STARTFILE_PREFIX`` is not searched when the compiler
  is built as a cross compiler.

.. c:macro:: STANDARD_STARTFILE_PREFIX_1

  Define this macro as a C string constant if you wish to override the
  standard choice of ``/lib`` as a prefix to try after the default prefix
  when searching for startup files such as :samp:`crt0.o`.
  ``STANDARD_STARTFILE_PREFIX_1`` is not searched when the compiler
  is built as a cross compiler.

.. c:macro:: STANDARD_STARTFILE_PREFIX_2

  Define this macro as a C string constant if you wish to override the
  standard choice of ``/lib`` as yet another prefix to try after the
  default prefix when searching for startup files such as :samp:`crt0.o`.
  ``STANDARD_STARTFILE_PREFIX_2`` is not searched when the compiler
  is built as a cross compiler.

.. c:macro:: MD_STARTFILE_PREFIX

  If defined, this macro supplies an additional prefix to try after the
  standard prefixes.  ``MD_EXEC_PREFIX`` is not searched when the
  compiler is built as a cross compiler.

.. c:macro:: MD_STARTFILE_PREFIX_1

  If defined, this macro supplies yet another prefix to try after the
  standard prefixes.  It is not searched when the compiler is built as a
  cross compiler.

.. c:macro:: INIT_ENVIRONMENT

  Define this macro as a C string constant if you wish to set environment
  variables for programs called by the driver, such as the assembler and
  loader.  The driver passes the value of this macro to ``putenv`` to
  initialize the necessary environment variables.

.. c:macro:: LOCAL_INCLUDE_DIR

  Define this macro as a C string constant if you wish to override the
  standard choice of :samp:`/usr/local/include` as the default prefix to
  try when searching for local header files.  ``LOCAL_INCLUDE_DIR``
  comes before ``NATIVE_SYSTEM_HEADER_DIR`` (set in
  :samp:`config.gcc`, normally :samp:`/usr/include`) in the search order.

  Cross compilers do not search either :samp:`/usr/local/include` or its
  replacement.

.. c:macro:: NATIVE_SYSTEM_HEADER_COMPONENT

  The 'component' corresponding to ``NATIVE_SYSTEM_HEADER_DIR``.
  See ``INCLUDE_DEFAULTS``, below, for the description of components.
  If you do not define this macro, no component is used.

.. c:macro:: INCLUDE_DEFAULTS

  Define this macro if you wish to override the entire default search path
  for include files.  For a native compiler, the default search path
  usually consists of ``GCC_INCLUDE_DIR``, ``LOCAL_INCLUDE_DIR``,
  ``GPLUSPLUS_INCLUDE_DIR``, and
  ``NATIVE_SYSTEM_HEADER_DIR``.  In addition, ``GPLUSPLUS_INCLUDE_DIR``
  and ``GCC_INCLUDE_DIR`` are defined automatically by :samp:`Makefile`,
  and specify private search areas for GCC.  The directory
  ``GPLUSPLUS_INCLUDE_DIR`` is used only for C++ programs.

  The definition should be an initializer for an array of structures.
  Each array element should have four elements: the directory name (a
  string constant), the component name (also a string constant), a flag
  for C++-only directories,
  and a flag showing that the includes in the directory don't need to be
  wrapped in ``extern C`` when compiling C++.  Mark the end of
  the array with a null element.

  The component name denotes what GNU package the include file is part of,
  if any, in all uppercase letters.  For example, it might be :samp:`GCC`
  or :samp:`BINUTILS`.  If the package is part of a vendor-supplied
  operating system, code the component name as :samp:`0`.

  For example, here is the definition used for VAX/VMS:

  .. code-block:: c++

    #define INCLUDE_DEFAULTS \
    {                                       \
      { "GNU_GXX_INCLUDE:", "G++", 1, 1},   \
      { "GNU_CC_INCLUDE:", "GCC", 0, 0},    \
      { "SYS$SYSROOT:[SYSLIB.]", 0, 0, 0},  \
      { ".", 0, 0, 0},                      \
      { 0, 0, 0, 0}                         \
    }

Here is the order of prefixes tried for exec files:

* Any prefixes specified by the user with :option:`-B`.

* The environment variable ``GCC_EXEC_PREFIX`` or, if ``GCC_EXEC_PREFIX``
  is not set and the compiler has not been installed in the configure-time
  :samp:`{prefix}`, the location in which the compiler has actually been installed.

* The directories specified by the environment variable ``COMPILER_PATH``.

* The macro ``STANDARD_EXEC_PREFIX``, if the compiler has been installed
  in the configured-time :samp:`{prefix}`.

* The location :samp:`/usr/libexec/gcc/`, but only if this is a native compiler.

* The location :samp:`/usr/lib/gcc/`, but only if this is a native compiler.

* The macro ``MD_EXEC_PREFIX``, if defined, but only if this is a native
  compiler.

Here is the order of prefixes tried for startfiles:

* Any prefixes specified by the user with :option:`-B`.

* The environment variable ``GCC_EXEC_PREFIX`` or its automatically determined
  value based on the installed toolchain location.

* The directories specified by the environment variable ``LIBRARY_PATH``
  (or port-specific name; native only, cross compilers do not use this).

* The macro ``STANDARD_EXEC_PREFIX``, but only if the toolchain is installed
  in the configured :samp:`{prefix}` or this is a native compiler.

* The location :samp:`/usr/lib/gcc/`, but only if this is a native compiler.

* The macro ``MD_EXEC_PREFIX``, if defined, but only if this is a native
  compiler.

* The macro ``MD_STARTFILE_PREFIX``, if defined, but only if this is a
  native compiler, or we have a target system root.

* The macro ``MD_STARTFILE_PREFIX_1``, if defined, but only if this is a
  native compiler, or we have a target system root.

* The macro ``STANDARD_STARTFILE_PREFIX``, with any sysroot modifications.
  If this path is relative it will be prefixed by ``GCC_EXEC_PREFIX`` and
  the machine suffix or ``STANDARD_EXEC_PREFIX`` and the machine suffix.

* The macro ``STANDARD_STARTFILE_PREFIX_1``, but only if this is a native
  compiler, or we have a target system root. The default for this macro is
  :samp:`/lib/`.

* The macro ``STANDARD_STARTFILE_PREFIX_2``, but only if this is a native
  compiler, or we have a target system root. The default for this macro is
  :samp:`/usr/lib/`.
