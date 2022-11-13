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

.. c:var:: const char * TARGET_EMUTLS_GET_ADDRESS

  .. hook-start:TARGET_EMUTLS_GET_ADDRESS

  Contains the name of the helper function that uses a TLS control
  object to locate a TLS instance.  The default causes libgcc's
  emulated TLS helper function to be used.

.. hook-end

.. c:var:: const char * TARGET_EMUTLS_REGISTER_COMMON

  .. hook-start:TARGET_EMUTLS_REGISTER_COMMON

  Contains the name of the helper function that should be used at
  program startup to register TLS objects that are implicitly
  initialized to zero.  If this is ``NULL``, all TLS objects will
  have explicit initializers.  The default causes libgcc's emulated TLS
  registration function to be used.

.. hook-end

.. c:var:: const char * TARGET_EMUTLS_VAR_SECTION

  .. hook-start:TARGET_EMUTLS_VAR_SECTION

  Contains the name of the section in which TLS control variables should
  be placed.  The default of ``NULL`` allows these to be placed in
  any section.

.. hook-end

.. c:var:: const char * TARGET_EMUTLS_TMPL_SECTION

  .. hook-start:TARGET_EMUTLS_TMPL_SECTION

  Contains the name of the section in which TLS initializers should be
  placed.  The default of ``NULL`` allows these to be placed in any
  section.

.. hook-end

.. c:var:: const char * TARGET_EMUTLS_VAR_PREFIX

  .. hook-start:TARGET_EMUTLS_VAR_PREFIX

  Contains the prefix to be prepended to TLS control variable names.
  The default of ``NULL`` uses a target-specific prefix.

.. hook-end

.. c:var:: const char * TARGET_EMUTLS_TMPL_PREFIX

  .. hook-start:TARGET_EMUTLS_TMPL_PREFIX

  Contains the prefix to be prepended to TLS initializer objects.  The
  default of ``NULL`` uses a target-specific prefix.

.. hook-end

.. function:: tree TARGET_EMUTLS_VAR_FIELDS (tree type, tree *name)

  .. hook-start:TARGET_EMUTLS_VAR_FIELDS

  Specifies a function that generates the FIELD_DECLs for a TLS control
  object type.  :samp:`{type}` is the RECORD_TYPE the fields are for and
  :samp:`{name}` should be filled with the structure tag, if the default of
  ``__emutls_object`` is unsuitable.  The default creates a type suitable
  for libgcc's emulated TLS function.

.. hook-end

.. function:: tree TARGET_EMUTLS_VAR_INIT (tree var, tree decl, tree tmpl_addr)

  .. hook-start:TARGET_EMUTLS_VAR_INIT

  Specifies a function that generates the CONSTRUCTOR to initialize a
  TLS control object.  :samp:`{var}` is the TLS control object, :samp:`{decl}`
  is the TLS object and :samp:`{tmpl_addr}` is the address of the
  initializer.  The default initializes libgcc's emulated TLS control object.

.. hook-end

.. c:var:: bool TARGET_EMUTLS_VAR_ALIGN_FIXED

  .. hook-start:TARGET_EMUTLS_VAR_ALIGN_FIXED

  Specifies whether the alignment of TLS control variable objects is
  fixed and should not be increased as some backends may do to optimize
  single objects.  The default is false.

.. hook-end

.. c:var:: bool TARGET_EMUTLS_DEBUG_FORM_TLS_ADDRESS

  .. hook-start:TARGET_EMUTLS_DEBUG_FORM_TLS_ADDRESS

  Specifies whether a DWARF ``DW_OP_form_tls_address`` location descriptor
  may be used to describe emulated TLS control objects.

.. hook-end