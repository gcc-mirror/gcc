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

.. c:var:: const struct attribute_spec * TARGET_ATTRIBUTE_TABLE

  .. hook-start:TARGET_ATTRIBUTE_TABLE

  If defined, this target hook points to an array of :samp:`struct
  attribute_spec` (defined in :samp:`tree-core.h`) specifying the machine
  specific attributes for this target and some of the restrictions on the
  entities to which these attributes are applied and the arguments they
  take.

.. hook-end

.. function:: bool TARGET_ATTRIBUTE_TAKES_IDENTIFIER_P (const_tree name)

  .. hook-start:TARGET_ATTRIBUTE_TAKES_IDENTIFIER_P

  If defined, this target hook is a function which returns true if the
  machine-specific attribute named :samp:`{name}` expects an identifier
  given as its first argument to be passed on as a plain identifier, not
  subjected to name lookup.  If this is not defined, the default is
  false for all machine-specific attributes.

.. hook-end

.. function:: int TARGET_COMP_TYPE_ATTRIBUTES (const_tree type1, const_tree type2)

  .. hook-start:TARGET_COMP_TYPE_ATTRIBUTES

  If defined, this target hook is a function which returns zero if the attributes on
  :samp:`{type1}` and :samp:`{type2}` are incompatible, one if they are compatible,
  and two if they are nearly compatible (which causes a warning to be
  generated).  If this is not defined, machine-specific attributes are
  supposed always to be compatible.

.. hook-end

.. function:: void TARGET_SET_DEFAULT_TYPE_ATTRIBUTES (tree type)

  .. hook-start:TARGET_SET_DEFAULT_TYPE_ATTRIBUTES

  If defined, this target hook is a function which assigns default attributes to
  the newly defined :samp:`{type}`.

.. hook-end

.. function:: tree TARGET_MERGE_TYPE_ATTRIBUTES (tree type1, tree type2)

  .. hook-start:TARGET_MERGE_TYPE_ATTRIBUTES

  Define this target hook if the merging of type attributes needs special
  handling.  If defined, the result is a list of the combined
  ``TYPE_ATTRIBUTES`` of :samp:`{type1}` and :samp:`{type2}`.  It is assumed
  that ``comptypes`` has already been called and returned 1.  This
  function may call ``merge_attributes`` to handle machine-independent
  merging.

.. hook-end

.. function:: tree TARGET_MERGE_DECL_ATTRIBUTES (tree olddecl, tree newdecl)

  .. hook-start:TARGET_MERGE_DECL_ATTRIBUTES

  Define this target hook if the merging of decl attributes needs special
  handling.  If defined, the result is a list of the combined
  ``DECL_ATTRIBUTES`` of :samp:`{olddecl}` and :samp:`{newdecl}`.
  :samp:`{newdecl}` is a duplicate declaration of :samp:`{olddecl}`.  Examples of
  when this is needed are when one attribute overrides another, or when an
  attribute is nullified by a subsequent definition.  This function may
  call ``merge_attributes`` to handle machine-independent merging.

  .. index:: TARGET_DLLIMPORT_DECL_ATTRIBUTES

  If the only target-specific handling you require is :samp:`dllimport`
  for Microsoft Windows targets, you should define the macro
  ``TARGET_DLLIMPORT_DECL_ATTRIBUTES`` to ``1``.  The compiler
  will then define a function called
  ``merge_dllimport_decl_attributes`` which can then be defined as
  the expansion of ``TARGET_MERGE_DECL_ATTRIBUTES``.  You can also
  add ``handle_dll_attribute`` in the attribute table for your port
  to perform initial processing of the :samp:`dllimport` and
  :samp:`dllexport` attributes.  This is done in :samp:`i386/cygwin.h` and
  :samp:`i386/i386.cc`, for example.

.. hook-end

.. function:: bool TARGET_VALID_DLLIMPORT_ATTRIBUTE_P (const_tree decl)

  .. hook-start:TARGET_VALID_DLLIMPORT_ATTRIBUTE_P

  :samp:`{decl}` is a variable or function with ``__attribute__((dllimport))``
  specified.  Use this hook if the target needs to add extra validation
  checks to ``handle_dll_attribute``.

.. hook-end

.. c:macro:: TARGET_DECLSPEC

  Define this macro to a nonzero value if you want to treat
  ``__declspec(X)`` as equivalent to ``__attribute((X))``.  By
  default, this behavior is enabled only for targets that define
  ``TARGET_DLLIMPORT_DECL_ATTRIBUTES``.  The current implementation
  of ``__declspec`` is via a built-in macro, but you should not rely
  on this implementation detail.

.. function:: void TARGET_INSERT_ATTRIBUTES (tree node, tree *attr_ptr)

  .. hook-start:TARGET_INSERT_ATTRIBUTES

  Define this target hook if you want to be able to add attributes to a decl
  when it is being created.  This is normally useful for back ends which
  wish to implement a pragma by using the attributes which correspond to
  the pragma's effect.  The :samp:`{node}` argument is the decl which is being
  created.  The :samp:`{attr_ptr}` argument is a pointer to the attribute list
  for this decl.  The list itself should not be modified, since it may be
  shared with other decls, but attributes may be chained on the head of
  the list and ``*attr_ptr`` modified to point to the new
  attributes, or a copy of the list may be made if further changes are
  needed.

.. hook-end

.. function:: tree TARGET_HANDLE_GENERIC_ATTRIBUTE (tree *node, tree name, tree args, int flags, bool *no_add_attrs)

  .. hook-start:TARGET_HANDLE_GENERIC_ATTRIBUTE

  Define this target hook if you want to be able to perform additional
  target-specific processing of an attribute which is handled generically
  by a front end.  The arguments are the same as those which are passed to
  attribute handlers.  So far this only affects the :samp:`{noinit}` and
  :samp:`{section}` attribute.

.. hook-end

.. function:: bool TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P (const_tree fndecl)

  .. hook-start:TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P

  .. index:: inlining

  This target hook returns ``true`` if it is OK to inline :samp:`{fndecl}`
  into the current function, despite its having target-specific
  attributes, ``false`` otherwise.  By default, if a function has a
  target specific attribute attached to it, it will not be inlined.

.. hook-end

.. function:: bool TARGET_OPTION_VALID_ATTRIBUTE_P (tree fndecl, tree name, tree args, int flags)

  .. hook-start:TARGET_OPTION_VALID_ATTRIBUTE_P

  This hook is called to parse ``attribute(target("..."))``, which
  allows setting target-specific options on individual functions.
  These function-specific options may differ
  from the options specified on the command line.  The hook should return
  ``true`` if the options are valid.

  The hook should set the ``DECL_FUNCTION_SPECIFIC_TARGET`` field in
  the function declaration to hold a pointer to a target-specific
  ``struct cl_target_option`` structure.

.. hook-end

.. function:: void TARGET_OPTION_SAVE (struct cl_target_option *ptr, struct gcc_options *opts, struct gcc_options *opts_set)

  .. hook-start:TARGET_OPTION_SAVE

  This hook is called to save any additional target-specific information
  in the ``struct cl_target_option`` structure for function-specific
  options from the ``struct gcc_options`` structure.
  See :ref:`option-file-format`.

.. hook-end

.. function:: void TARGET_OPTION_RESTORE (struct gcc_options *opts, struct gcc_options *opts_set, struct cl_target_option *ptr)

  .. hook-start:TARGET_OPTION_RESTORE

  This hook is called to restore any additional target-specific
  information in the ``struct cl_target_option`` structure for
  function-specific options to the ``struct gcc_options`` structure.

.. hook-end

.. function:: void TARGET_OPTION_POST_STREAM_IN (struct cl_target_option *ptr)

  .. hook-start:TARGET_OPTION_POST_STREAM_IN

  This hook is called to update target-specific information in the
  ``struct cl_target_option`` structure after it is streamed in from
  LTO bytecode.

.. hook-end

.. function:: void TARGET_OPTION_PRINT (FILE *file, int indent, struct cl_target_option *ptr)

  .. hook-start:TARGET_OPTION_PRINT

  This hook is called to print any additional target-specific
  information in the ``struct cl_target_option`` structure for
  function-specific options.

.. hook-end

.. function:: bool TARGET_OPTION_PRAGMA_PARSE (tree args, tree pop_target)

  .. hook-start:TARGET_OPTION_PRAGMA_PARSE

  This target hook parses the options for ``#pragma GCC target``, which
  sets the target-specific options for functions that occur later in the
  input stream.  The options accepted should be the same as those handled by the
  ``TARGET_OPTION_VALID_ATTRIBUTE_P`` hook.

.. hook-end

.. function:: void TARGET_OPTION_OVERRIDE (void)

  .. hook-start:TARGET_OPTION_OVERRIDE

  Sometimes certain combinations of command options do not make sense on
  a particular target machine.  You can override the hook
  ``TARGET_OPTION_OVERRIDE`` to take account of this.  This hooks is called
  once just after all the command options have been parsed.

  Don't use this hook to turn on various extra optimizations for
  :option:`-O`.  That is what ``TARGET_OPTION_OPTIMIZATION`` is for.

  If you need to do something whenever the optimization level is
  changed via the optimize attribute or pragma, see
  ``TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE``

.. hook-end

.. function:: bool TARGET_OPTION_FUNCTION_VERSIONS (tree decl1, tree decl2)

  .. hook-start:TARGET_OPTION_FUNCTION_VERSIONS

  This target hook returns ``true`` if :samp:`{DECL1}` and :samp:`{DECL2}` are
  versions of the same function.  :samp:`{DECL1}` and :samp:`{DECL2}` are function
  versions if and only if they have the same function signature and
  different target specific attributes, that is, they are compiled for
  different target machines.

.. hook-end

.. function:: bool TARGET_CAN_INLINE_P (tree caller, tree callee)

  .. hook-start:TARGET_CAN_INLINE_P

  This target hook returns ``false`` if the :samp:`{caller}` function
  cannot inline :samp:`{callee}`, based on target specific information.  By
  default, inlining is not allowed if the callee function has function
  specific target options and the caller does not use the same options.

.. hook-end

.. function:: bool TARGET_UPDATE_IPA_FN_TARGET_INFO (unsigned int& info, const gimple* stmt)

   .. hook-start:TARGET_UPDATE_IPA_FN_TARGET_INFO

  Allow target to analyze all gimple statements for the given function to
  record and update some target specific information for inlining.  A typical
  example is that a caller with one isa feature disabled is normally not
  allowed to inline a callee with that same isa feature enabled even which is
  attributed by always_inline, but with the conservative analysis on all
  statements of the callee if we are able to guarantee the callee does not
  exploit any instructions from the mismatch isa feature, it would be safe to
  allow the caller to inline the callee.
  :samp:`{info}` is one ``unsigned int`` value to record information in which
  one set bit indicates one corresponding feature is detected in the analysis,
  :samp:`{stmt}` is the statement being analyzed.  Return true if target still
  need to analyze the subsequent statements, otherwise return false to stop
  subsequent analysis.
  The default version of this hook returns false.

.. hook-end

.. function:: bool TARGET_NEED_IPA_FN_TARGET_INFO (const_tree decl, unsigned int& info)

  .. hook-start:TARGET_NEED_IPA_FN_TARGET_INFO

  Allow target to check early whether it is necessary to analyze all gimple
  statements in the given function to update target specific information for
  inlining.  See hook ``update_ipa_fn_target_info`` for usage example of
  target specific information.  This hook is expected to be invoked ahead of
  the iterating with hook ``update_ipa_fn_target_info``.
  :samp:`{decl}` is the function being analyzed, :samp:`{info}` is the same as what
  in hook ``update_ipa_fn_target_info``, target can do one time update
  into :samp:`{info}` without iterating for some case.  Return true if target
  decides to analyze all gimple statements to collect information, otherwise
  return false.
  The default version of this hook returns false.

.. hook-end

.. function:: void TARGET_RELAYOUT_FUNCTION (tree fndecl)

  .. hook-start:TARGET_RELAYOUT_FUNCTION

  This target hook fixes function :samp:`{fndecl}` after attributes are processed.
  Default does nothing. On ARM, the default function's alignment is updated
  with the attribute target.

.. hook-end