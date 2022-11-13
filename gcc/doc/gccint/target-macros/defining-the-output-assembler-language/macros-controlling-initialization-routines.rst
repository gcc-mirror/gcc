..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _macros-for-initialization:

Macros Controlling Initialization Routines
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here are the macros that control how the compiler handles initialization
and termination functions:

.. c:macro:: INIT_SECTION_ASM_OP

  If defined, a C string constant, including spacing, for the assembler
  operation to identify the following data as initialization code.  If not
  defined, GCC will assume such a section does not exist.  When you are
  using special sections for initialization and termination functions, this
  macro also controls how :samp:`crtstuff.c` and :samp:`libgcc2.c` arrange to
  run the initialization functions.

.. c:macro:: HAS_INIT_SECTION

  If defined, ``main`` will not call ``__main`` as described above.
  This macro should be defined for systems that control start-up code
  on a symbol-by-symbol basis, such as OSF/1, and should not
  be defined explicitly for systems that support ``INIT_SECTION_ASM_OP``.

.. c:macro:: LD_INIT_SWITCH

  If defined, a C string constant for a switch that tells the linker that
  the following symbol is an initialization routine.

.. c:macro:: LD_FINI_SWITCH

  If defined, a C string constant for a switch that tells the linker that
  the following symbol is a finalization routine.

.. c:macro:: COLLECT_SHARED_INIT_FUNC (stream, func)

  If defined, a C statement that will write a function that can be
  automatically called when a shared library is loaded.  The function
  should call :samp:`{func}`, which takes no arguments.  If not defined, and
  the object format requires an explicit initialization function, then a
  function called ``_GLOBAL__DI`` will be generated.

  This function and the following one are used by collect2 when linking a
  shared library that needs constructors or destructors, or has DWARF2
  exception tables embedded in the code.

.. c:macro:: COLLECT_SHARED_FINI_FUNC (stream, func)

  If defined, a C statement that will write a function that can be
  automatically called when a shared library is unloaded.  The function
  should call :samp:`{func}`, which takes no arguments.  If not defined, and
  the object format requires an explicit finalization function, then a
  function called ``_GLOBAL__DD`` will be generated.

.. c:macro:: INVOKE__main

  If defined, ``main`` will call ``__main`` despite the presence of
  ``INIT_SECTION_ASM_OP``.  This macro should be defined for systems
  where the init section is not actually run automatically, but is still
  useful for collecting the lists of constructors and destructors.

.. c:macro:: SUPPORTS_INIT_PRIORITY

  If nonzero, the C++ ``init_priority`` attribute is supported and the
  compiler should emit instructions to control the order of initialization
  of objects.  If zero, the compiler will issue an error message upon
  encountering an ``init_priority`` attribute.

.. c:var:: bool TARGET_HAVE_CTORS_DTORS

  .. hook-start:TARGET_HAVE_CTORS_DTORS

  This value is true if the target supports some 'native' method of
  collecting constructors and destructors to be run at startup and exit.
  It is false if we must use :command:`collect2`.

.. hook-end

.. c:var:: bool TARGET_DTORS_FROM_CXA_ATEXIT

  .. hook-start:TARGET_DTORS_FROM_CXA_ATEXIT

  This value is true if the target wants destructors to be queued to be
  run from __cxa_atexit.  If this is the case then, for each priority level,
  a new constructor will be entered that registers the destructors for that
  level with __cxa_atexit (and there will be no destructors emitted).
  It is false the method implied by ``have_ctors_dtors`` is used.

.. hook-end

.. function:: void TARGET_ASM_CONSTRUCTOR (rtx symbol, int priority)

  .. hook-start:TARGET_ASM_CONSTRUCTOR

  If defined, a function that outputs assembler code to arrange to call
  the function referenced by :samp:`{symbol}` at initialization time.

  Assume that :samp:`{symbol}` is a ``SYMBOL_REF`` for a function taking
  no arguments and with no return value.  If the target supports initialization
  priorities, :samp:`{priority}` is a value between 0 and ``MAX_INIT_PRIORITY`` ;
  otherwise it must be ``DEFAULT_INIT_PRIORITY``.

  If this macro is not defined by the target, a suitable default will
  be chosen if (1) the target supports arbitrary section names, (2) the
  target defines ``CTORS_SECTION_ASM_OP``, or (3) ``USE_COLLECT2``
  is not defined.

.. hook-end

.. function:: void TARGET_ASM_DESTRUCTOR (rtx symbol, int priority)

  .. hook-start:TARGET_ASM_DESTRUCTOR

  This is like ``TARGET_ASM_CONSTRUCTOR`` but used for termination
  functions rather than initialization functions.

.. hook-end

If ``TARGET_HAVE_CTORS_DTORS`` is true, the initialization routine
generated for the generated object file will have static linkage.

If your system uses :command:`collect2` as the means of processing
constructors, then that program normally uses :command:`nm` to scan
an object file for constructor functions to be called.

On certain kinds of systems, you can define this macro to make
:command:`collect2` work faster (and, in some cases, make it work at all):

.. c:macro:: OBJECT_FORMAT_COFF

  Define this macro if the system uses COFF (Common Object File Format)
  object files, so that :command:`collect2` can assume this format and scan
  object files directly for dynamic constructor/destructor functions.

  This macro is effective only in a native compiler; :command:`collect2` as
  part of a cross compiler always uses :command:`nm` for the target machine.

.. c:macro:: REAL_NM_FILE_NAME

  Define this macro as a C string constant containing the file name to use
  to execute :command:`nm`.  The default is to search the path normally for
  :command:`nm`.

.. c:macro:: NM_FLAGS

  :command:`collect2` calls :command:`nm` to scan object files for static
  constructors and destructors and LTO info.  By default, :option:`-n` is
  passed.  Define ``NM_FLAGS`` to a C string constant if other options
  are needed to get the same output format as GNU :command:`nm -n`
  produces.

If your system supports shared libraries and has a program to list the
dynamic dependencies of a given library or executable, you can define
these macros to enable support for running initialization and
termination functions in shared libraries:

.. c:macro:: LDD_SUFFIX

  Define this macro to a C string constant containing the name of the program
  which lists dynamic dependencies, like :command:`ldd` under SunOS 4.

.. c:macro:: PARSE_LDD_OUTPUT (ptr)

  Define this macro to be C code that extracts filenames from the output
  of the program denoted by ``LDD_SUFFIX``.  :samp:`{ptr}` is a variable
  of type ``char *`` that points to the beginning of a line of output
  from ``LDD_SUFFIX``.  If the line lists a dynamic dependency, the
  code must advance :samp:`{ptr}` to the beginning of the filename on that
  line.  Otherwise, it must set :samp:`{ptr}` to ``NULL``.

.. c:macro:: SHLIB_SUFFIX

  Define this macro to a C string constant containing the default shared
  library extension of the target (e.g., :samp:`".so"`).  :command:`collect2`
  strips version information after this suffix when generating global
  constructor and destructor names.  This define is only needed on targets
  that use :command:`collect2` to process constructors and destructors.