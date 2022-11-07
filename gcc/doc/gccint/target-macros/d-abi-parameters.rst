..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: parameters, d abi

.. _d-language-and-abi:

D ABI parameters
****************

.. function:: void TARGET_D_CPU_VERSIONS (void)

  .. hook-start:TARGET_D_CPU_VERSIONS

  Declare all environmental version identifiers relating to the target CPU
  using the function ``builtin_version``, which takes a string representing
  the name of the version.  Version identifiers predefined by this hook apply
  to all modules that are being compiled and imported.

.. hook-end

.. function:: void TARGET_D_OS_VERSIONS (void)

  .. hook-start:TARGET_D_OS_VERSIONS

  Similarly to ``TARGET_D_CPU_VERSIONS``, but is used for versions
  relating to the target operating system.

.. hook-end

.. function:: void TARGET_D_REGISTER_CPU_TARGET_INFO (void)

  .. hook-start:TARGET_D_REGISTER_CPU_TARGET_INFO

  Register all target information keys relating to the target CPU using the
  function ``d_add_target_info_handlers``, which takes a
  :samp:`struct d_target_info_spec` (defined in :samp:`d/d-target.h`).  The keys
  added by this hook are made available at compile time by the
  ``__traits(getTargetInfo)`` extension, the result is an expression
  describing the requested target information.

.. hook-end

.. function:: void TARGET_D_REGISTER_OS_TARGET_INFO (void)

  .. hook-start:TARGET_D_REGISTER_OS_TARGET_INFO

  Same as ``TARGET_D_CPU_TARGET_INFO``, but is used for keys relating to
  the target operating system.

.. hook-end

.. c:var:: const char * TARGET_D_MINFO_SECTION

  .. hook-start:TARGET_D_MINFO_SECTION

  Contains the name of the section in which module info references should be
  placed.  By default, the compiler puts all module info symbols in the
  ``"minfo"`` section.  Define this macro to override the string if a
  different section name should be used.  This section is expected to be
  bracketed by two symbols ``TARGET_D_MINFO_SECTION_START`` and 
  ``TARGET_D_MINFO_SECTION_END`` to indicate the start and end address of
  the section, so that the runtime library can collect all modules for each
  loaded shared library and executable.  Setting the value to ``NULL``
  disables the use of sections for storing module info altogether.

.. hook-end

.. c:var:: const char * TARGET_D_MINFO_SECTION_START

  .. hook-start:TARGET_D_MINFO_SECTION_START

  If ``TARGET_D_MINFO_SECTION`` is defined, then this must also be defined
  as the name of the symbol indicating the start address of the module info
  section

.. hook-end

.. c:var:: const char * TARGET_D_MINFO_SECTION_END

  .. hook-start:TARGET_D_MINFO_SECTION_END

  If ``TARGET_D_MINFO_SECTION`` is defined, then this must also be defined
  as the name of the symbol indicating the end address of the module info
  section

.. hook-end

.. function:: bool TARGET_D_HAS_STDCALL_CONVENTION (unsigned int *link_system, unsigned int *link_windows)

  .. hook-start:TARGET_D_HAS_STDCALL_CONVENTION

  Returns ``true`` if the target supports the stdcall calling convention.
  The hook should also set :samp:`{link_system}` to ``1`` if the ``stdcall``
  attribute should be applied to functions with ``extern(System)`` linkage,
  and :samp:`{link_windows}` to ``1`` to apply ``stdcall`` to functions with
  ``extern(Windows)`` linkage.

.. hook-end

.. c:var:: bool TARGET_D_TEMPLATES_ALWAYS_COMDAT

  .. hook-start:TARGET_D_TEMPLATES_ALWAYS_COMDAT

  This flag is true if instantiated functions and variables are always COMDAT
  if they have external linkage.  If this flag is false, then instantiated
  decls will be emitted as weak symbols.  The default is ``false``.

.. hook-end
