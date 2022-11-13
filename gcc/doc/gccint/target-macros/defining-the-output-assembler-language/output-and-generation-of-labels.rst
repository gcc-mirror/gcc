..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _label-output:

Output and Generation of Labels
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This is about outputting labels.

.. index:: assemble_name

.. c:macro:: ASM_OUTPUT_LABEL (stream, name)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` the assembler definition of a label named :samp:`{name}`.
  Use the expression ``assemble_name (stream, name)`` to
  output the name itself; before and after that, output the additional
  assembler syntax for defining the name, and a newline.  A default
  definition of this macro is provided which is correct for most systems.

.. c:macro:: ASM_OUTPUT_FUNCTION_LABEL (stream, name, decl)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` the assembler definition of a label named :samp:`{name}` of
  a function.
  Use the expression ``assemble_name (stream, name)`` to
  output the name itself; before and after that, output the additional
  assembler syntax for defining the name, and a newline.  A default
  definition of this macro is provided which is correct for most systems.

  If this macro is not defined, then the function name is defined in the
  usual manner as a label (by means of ``ASM_OUTPUT_LABEL``).

.. index:: assemble_name_raw

.. c:macro:: ASM_OUTPUT_INTERNAL_LABEL (stream, name)

  Identical to ``ASM_OUTPUT_LABEL``, except that :samp:`{name}` is known
  to refer to a compiler-generated label.  The default definition uses
  ``assemble_name_raw``, which is like ``assemble_name`` except
  that it is more efficient.

.. c:macro:: SIZE_ASM_OP

  A C string containing the appropriate assembler directive to specify the
  size of a symbol, without any arguments.  On systems that use ELF, the
  default (in :samp:`config/elfos.h`) is :samp:`"\\t.size\\t"`; on other
  systems, the default is not to define this macro.

  Define this macro only if it is correct to use the default definitions
  of ``ASM_OUTPUT_SIZE_DIRECTIVE`` and ``ASM_OUTPUT_MEASURED_SIZE``
  for your system.  If you need your own custom definitions of those
  macros, or if you do not need explicit symbol sizes at all, do not
  define this macro.

.. c:macro:: ASM_OUTPUT_SIZE_DIRECTIVE (stream, name, size)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` a directive telling the assembler that the size of the
  symbol :samp:`{name}` is :samp:`{size}`.  :samp:`{size}` is a ``HOST_WIDE_INT``.
  If you define ``SIZE_ASM_OP``, a default definition of this macro is
  provided.

.. c:macro:: ASM_OUTPUT_MEASURED_SIZE (stream, name)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` a directive telling the assembler to calculate the size of
  the symbol :samp:`{name}` by subtracting its address from the current
  address.

  If you define ``SIZE_ASM_OP``, a default definition of this macro is
  provided.  The default assumes that the assembler recognizes a special
  :samp:`.` symbol as referring to the current address, and can calculate
  the difference between this and another symbol.  If your assembler does
  not recognize :samp:`.` or cannot do calculations with it, you will need
  to redefine ``ASM_OUTPUT_MEASURED_SIZE`` to use some other technique.

.. c:macro:: NO_DOLLAR_IN_LABEL

  Define this macro if the assembler does not accept the character
  :samp:`$` in label names.  By default constructors and destructors in
  G++ have :samp:`$` in the identifiers.  If this macro is defined,
  :samp:`.` is used instead.

.. c:macro:: NO_DOT_IN_LABEL

  Define this macro if the assembler does not accept the character
  :samp:`.` in label names.  By default constructors and destructors in G++
  have names that use :samp:`.`.  If this macro is defined, these names
  are rewritten to avoid :samp:`.`.

.. c:macro:: TYPE_ASM_OP

  A C string containing the appropriate assembler directive to specify the
  type of a symbol, without any arguments.  On systems that use ELF, the
  default (in :samp:`config/elfos.h`) is :samp:`"\\t.type\\t"`; on other
  systems, the default is not to define this macro.

  Define this macro only if it is correct to use the default definition of
  ``ASM_OUTPUT_TYPE_DIRECTIVE`` for your system.  If you need your own
  custom definition of this macro, or if you do not need explicit symbol
  types at all, do not define this macro.

.. c:macro:: TYPE_OPERAND_FMT

  A C string which specifies (using ``printf`` syntax) the format of
  the second operand to ``TYPE_ASM_OP``.  On systems that use ELF, the
  default (in :samp:`config/elfos.h`) is :samp:`"@%s"`; on other systems,
  the default is not to define this macro.

  Define this macro only if it is correct to use the default definition of
  ``ASM_OUTPUT_TYPE_DIRECTIVE`` for your system.  If you need your own
  custom definition of this macro, or if you do not need explicit symbol
  types at all, do not define this macro.

.. c:macro:: ASM_OUTPUT_TYPE_DIRECTIVE (stream, type)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` a directive telling the assembler that the type of the
  symbol :samp:`{name}` is :samp:`{type}`.  :samp:`{type}` is a C string; currently,
  that string is always either :samp:`"function"` or :samp:`"object"`, but
  you should not count on this.

  If you define ``TYPE_ASM_OP`` and ``TYPE_OPERAND_FMT``, a default
  definition of this macro is provided.

.. c:macro:: ASM_DECLARE_FUNCTION_NAME (stream, name, decl)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` any text necessary for declaring the name :samp:`{name}` of a
  function which is being defined.  This macro is responsible for
  outputting the label definition (perhaps using
  ``ASM_OUTPUT_FUNCTION_LABEL``).  The argument :samp:`{decl}` is the
  ``FUNCTION_DECL`` tree node representing the function.

  If this macro is not defined, then the function name is defined in the
  usual manner as a label (by means of ``ASM_OUTPUT_FUNCTION_LABEL``).

  You may wish to use ``ASM_OUTPUT_TYPE_DIRECTIVE`` in the definition
  of this macro.

.. c:macro:: ASM_DECLARE_FUNCTION_SIZE (stream, name, decl)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` any text necessary for declaring the size of a function
  which is being defined.  The argument :samp:`{name}` is the name of the
  function.  The argument :samp:`{decl}` is the ``FUNCTION_DECL`` tree node
  representing the function.

  If this macro is not defined, then the function size is not defined.

  You may wish to use ``ASM_OUTPUT_MEASURED_SIZE`` in the definition
  of this macro.

.. c:macro:: ASM_DECLARE_COLD_FUNCTION_NAME (stream, name, decl)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` any text necessary for declaring the name :samp:`{name}` of a
  cold function partition which is being defined.  This macro is responsible
  for outputting the label definition (perhaps using
  ``ASM_OUTPUT_FUNCTION_LABEL``).  The argument :samp:`{decl}` is the
  ``FUNCTION_DECL`` tree node representing the function.

  If this macro is not defined, then the cold partition name is defined in the
  usual manner as a label (by means of ``ASM_OUTPUT_LABEL``).

  You may wish to use ``ASM_OUTPUT_TYPE_DIRECTIVE`` in the definition
  of this macro.

.. c:macro:: ASM_DECLARE_COLD_FUNCTION_SIZE (stream, name, decl)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` any text necessary for declaring the size of a cold function
  partition which is being defined.  The argument :samp:`{name}` is the name of the
  cold partition of the function.  The argument :samp:`{decl}` is the
  ``FUNCTION_DECL`` tree node representing the function.

  If this macro is not defined, then the partition size is not defined.

  You may wish to use ``ASM_OUTPUT_MEASURED_SIZE`` in the definition
  of this macro.

.. c:macro:: ASM_DECLARE_OBJECT_NAME (stream, name, decl)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` any text necessary for declaring the name :samp:`{name}` of an
  initialized variable which is being defined.  This macro must output the
  label definition (perhaps using ``ASM_OUTPUT_LABEL``).  The argument
  :samp:`{decl}` is the ``VAR_DECL`` tree node representing the variable.

  If this macro is not defined, then the variable name is defined in the
  usual manner as a label (by means of ``ASM_OUTPUT_LABEL``).

  You may wish to use ``ASM_OUTPUT_TYPE_DIRECTIVE`` and/or
  ``ASM_OUTPUT_SIZE_DIRECTIVE`` in the definition of this macro.

.. function:: void TARGET_ASM_DECLARE_CONSTANT_NAME (FILE *file, const char *name, const_tree expr, HOST_WIDE_INT size)

  .. hook-start:TARGET_ASM_DECLARE_CONSTANT_NAME

  A target hook to output to the stdio stream :samp:`{file}` any text necessary
  for declaring the name :samp:`{name}` of a constant which is being defined.  This
  target hook is responsible for outputting the label definition (perhaps using
  ``assemble_label``).  The argument :samp:`{exp}` is the value of the constant,
  and :samp:`{size}` is the size of the constant in bytes.  The :samp:`{name}`
  will be an internal label.

  The default version of this target hook, define the :samp:`{name}` in the
  usual manner as a label (by means of ``assemble_label``).

  You may wish to use ``ASM_OUTPUT_TYPE_DIRECTIVE`` in this target hook.

.. hook-end

.. c:macro:: ASM_DECLARE_REGISTER_GLOBAL (stream, decl, regno, name)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` any text necessary for claiming a register :samp:`{regno}`
  for a global variable :samp:`{decl}` with name :samp:`{name}`.

  If you don't define this macro, that is equivalent to defining it to do
  nothing.

.. c:macro:: ASM_FINISH_DECLARE_OBJECT (stream, decl, toplevel, atend)

  A C statement (sans semicolon) to finish up declaring a variable name
  once the compiler has processed its initializer fully and thus has had a
  chance to determine the size of an array when controlled by an
  initializer.  This is used on systems where it's necessary to declare
  something about the size of the object.

  If you don't define this macro, that is equivalent to defining it to do
  nothing.

  You may wish to use ``ASM_OUTPUT_SIZE_DIRECTIVE`` and/or
  ``ASM_OUTPUT_MEASURED_SIZE`` in the definition of this macro.

.. function:: void TARGET_ASM_GLOBALIZE_LABEL (FILE *stream, const char *name)

  .. hook-start:TARGET_ASM_GLOBALIZE_LABEL

  This target hook is a function to output to the stdio stream
  :samp:`{stream}` some commands that will make the label :samp:`{name}` global;
  that is, available for reference from other files.

  The default implementation relies on a proper definition of
  ``GLOBAL_ASM_OP``.

.. hook-end

.. function:: void TARGET_ASM_GLOBALIZE_DECL_NAME (FILE *stream, tree decl)

  .. hook-start:TARGET_ASM_GLOBALIZE_DECL_NAME

  This target hook is a function to output to the stdio stream
  :samp:`{stream}` some commands that will make the name associated with :samp:`{decl}`
  global; that is, available for reference from other files.

  The default implementation uses the TARGET_ASM_GLOBALIZE_LABEL target hook.

.. hook-end

.. function:: void TARGET_ASM_ASSEMBLE_UNDEFINED_DECL (FILE *stream, const char *name, const_tree decl)

  .. hook-start:TARGET_ASM_ASSEMBLE_UNDEFINED_DECL

  This target hook is a function to output to the stdio stream
  :samp:`{stream}` some commands that will declare the name associated with
  :samp:`{decl}` which is not defined in the current translation unit.  Most
  assemblers do not require anything to be output in this case.

.. hook-end

.. c:macro:: ASM_WEAKEN_LABEL (stream, name)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` some commands that will make the label :samp:`{name}` weak;
  that is, available for reference from other files but only used if
  no other definition is available.  Use the expression
  ``assemble_name (stream, name)`` to output the name
  itself; before and after that, output the additional assembler syntax
  for making that name weak, and a newline.

  If you don't define this macro or ``ASM_WEAKEN_DECL``, GCC will not
  support weak symbols and you should not define the ``SUPPORTS_WEAK``
  macro.

.. c:macro:: ASM_WEAKEN_DECL (stream, decl, name, value)

  Combines (and replaces) the function of ``ASM_WEAKEN_LABEL`` and
  ``ASM_OUTPUT_WEAK_ALIAS``, allowing access to the associated function
  or variable decl.  If :samp:`{value}` is not ``NULL``, this C statement
  should output to the stdio stream :samp:`{stream}` assembler code which
  defines (equates) the weak symbol :samp:`{name}` to have the value
  :samp:`{value}`.  If :samp:`{value}` is ``NULL``, it should output commands
  to make :samp:`{name}` weak.

.. c:macro:: ASM_OUTPUT_WEAKREF (stream, decl, name, value)

  Outputs a directive that enables :samp:`{name}` to be used to refer to
  symbol :samp:`{value}` with weak-symbol semantics.  ``decl`` is the
  declaration of ``name``.

.. c:macro:: SUPPORTS_WEAK

  A preprocessor constant expression which evaluates to true if the target
  supports weak symbols.

  If you don't define this macro, :samp:`defaults.h` provides a default
  definition.  If either ``ASM_WEAKEN_LABEL`` or ``ASM_WEAKEN_DECL``
  is defined, the default definition is :samp:`1`; otherwise, it is :samp:`0`.

.. c:macro:: TARGET_SUPPORTS_WEAK

  A C expression which evaluates to true if the target supports weak symbols.

  If you don't define this macro, :samp:`defaults.h` provides a default
  definition.  The default definition is :samp:`(SUPPORTS_WEAK)`.  Define
  this macro if you want to control weak symbol support with a compiler
  flag such as :option:`-melf`.

.. c:macro:: MAKE_DECL_ONE_ONLY (decl)

  A C statement (sans semicolon) to mark :samp:`{decl}` to be emitted as a
  public symbol such that extra copies in multiple translation units will
  be discarded by the linker.  Define this macro if your object file
  format provides support for this concept, such as the :samp:`COMDAT`
  section flags in the Microsoft Windows PE/COFF format, and this support
  requires changes to :samp:`{decl}`, such as putting it in a separate section.

.. c:macro:: SUPPORTS_ONE_ONLY

  A C expression which evaluates to true if the target supports one-only
  semantics.

  If you don't define this macro, :samp:`varasm.cc` provides a default
  definition.  If ``MAKE_DECL_ONE_ONLY`` is defined, the default
  definition is :samp:`1`; otherwise, it is :samp:`0`.  Define this macro if
  you want to control one-only symbol support with a compiler flag, or if
  setting the ``DECL_ONE_ONLY`` flag is enough to mark a declaration to
  be emitted as one-only.

.. function:: void TARGET_ASM_ASSEMBLE_VISIBILITY (tree decl, int visibility)

  .. hook-start:TARGET_ASM_ASSEMBLE_VISIBILITY

  This target hook is a function to output to :samp:`{asm_out_file}` some
  commands that will make the symbol(s) associated with :samp:`{decl}` have
  hidden, protected or internal visibility as specified by :samp:`{visibility}`.

.. hook-end

.. c:macro:: TARGET_WEAK_NOT_IN_ARCHIVE_TOC

  A C expression that evaluates to true if the target's linker expects
  that weak symbols do not appear in a static archive's table of contents.
  The default is ``0``.

  Leaving weak symbols out of an archive's table of contents means that,
  if a symbol will only have a definition in one translation unit and
  will have undefined references from other translation units, that
  symbol should not be weak.  Defining this macro to be nonzero will
  thus have the effect that certain symbols that would normally be weak
  (explicit template instantiations, and vtables for polymorphic classes
  with noninline key methods) will instead be nonweak.

  The C++ ABI requires this macro to be zero.  Define this macro for
  targets where full C++ ABI compliance is impossible and where linker
  restrictions require weak symbols to be left out of a static archive's
  table of contents.

.. c:macro:: ASM_OUTPUT_EXTERNAL (stream, decl, name)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` any text necessary for declaring the name of an external
  symbol named :samp:`{name}` which is referenced in this compilation but
  not defined.  The value of :samp:`{decl}` is the tree node for the
  declaration.

  This macro need not be defined if it does not need to output anything.
  The GNU assembler and most Unix assemblers don't require anything.

.. function:: void TARGET_ASM_EXTERNAL_LIBCALL (rtx symref)

  .. hook-start:TARGET_ASM_EXTERNAL_LIBCALL

  This target hook is a function to output to :samp:`{asm_out_file}` an assembler
  pseudo-op to declare a library function name external.  The name of the
  library function is given by :samp:`{symref}`, which is a ``symbol_ref``.

.. hook-end

.. function:: void TARGET_ASM_MARK_DECL_PRESERVED (const char *symbol)

  .. hook-start:TARGET_ASM_MARK_DECL_PRESERVED

  This target hook is a function to output to :samp:`{asm_out_file}` an assembler
  directive to annotate :samp:`{symbol}` as used.  The Darwin target uses the
  .no_dead_code_strip directive.

.. hook-end

.. c:macro:: ASM_OUTPUT_LABELREF (stream, name)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` a reference in assembler syntax to a label named
  :samp:`{name}`.  This should add :samp:`_` to the front of the name, if that
  is customary on your operating system, as it is in most Berkeley Unix
  systems.  This macro is used in ``assemble_name``.

.. function:: tree TARGET_MANGLE_ASSEMBLER_NAME (const char *name)

  .. hook-start:TARGET_MANGLE_ASSEMBLER_NAME

  Given a symbol :samp:`{name}`, perform same mangling as ``varasm.cc`` 's
  ``assemble_name``, but in memory rather than to a file stream, returning
  result as an ``IDENTIFIER_NODE``.  Required for correct LTO symtabs.  The
  default implementation calls the ``TARGET_STRIP_NAME_ENCODING`` hook and
  then prepends the ``USER_LABEL_PREFIX``, if any.

.. hook-end

.. c:macro:: ASM_OUTPUT_SYMBOL_REF (stream, sym)

  A C statement (sans semicolon) to output a reference to
  ``SYMBOL_REF`` :samp:`{sym}`.  If not defined, ``assemble_name``
  will be used to output the name of the symbol.  This macro may be used
  to modify the way a symbol is referenced depending on information
  encoded by ``TARGET_ENCODE_SECTION_INFO``.

.. c:macro:: ASM_OUTPUT_LABEL_REF (stream, buf)

  A C statement (sans semicolon) to output a reference to :samp:`{buf}`, the
  result of ``ASM_GENERATE_INTERNAL_LABEL``.  If not defined,
  ``assemble_name`` will be used to output the name of the symbol.
  This macro is not used by ``output_asm_label``, or the ``%l``
  specifier that calls it; the intention is that this macro should be set
  when it is necessary to output a label differently when its address is
  being taken.

.. function:: void TARGET_ASM_INTERNAL_LABEL (FILE *stream, const char *prefix, unsigned long labelno)

  .. hook-start:TARGET_ASM_INTERNAL_LABEL

  A function to output to the stdio stream :samp:`{stream}` a label whose
  name is made from the string :samp:`{prefix}` and the number :samp:`{labelno}`.

  It is absolutely essential that these labels be distinct from the labels
  used for user-level functions and variables.  Otherwise, certain programs
  will have name conflicts with internal labels.

  It is desirable to exclude internal labels from the symbol table of the
  object file.  Most assemblers have a naming convention for labels that
  should be excluded; on many systems, the letter :samp:`L` at the
  beginning of a label has this effect.  You should find out what
  convention your system uses, and follow it.

  The default version of this function utilizes ``ASM_GENERATE_INTERNAL_LABEL``.

.. hook-end

.. c:macro:: ASM_OUTPUT_DEBUG_LABEL (stream, prefix, num)

  A C statement to output to the stdio stream :samp:`{stream}` a debug info
  label whose name is made from the string :samp:`{prefix}` and the number
  :samp:`{num}`.  This is useful for VLIW targets, where debug info labels
  may need to be treated differently than branch target labels.  On some
  systems, branch target labels must be at the beginning of instruction
  bundles, but debug info labels can occur in the middle of instruction
  bundles.

  If this macro is not defined, then ``(*targetm.asm_out.internal_label)`` will be
  used.

.. c:macro:: ASM_GENERATE_INTERNAL_LABEL (string, prefix, num)

  A C statement to store into the string :samp:`{string}` a label whose name
  is made from the string :samp:`{prefix}` and the number :samp:`{num}`.

  This string, when output subsequently by ``assemble_name``, should
  produce the output that ``(*targetm.asm_out.internal_label)`` would produce
  with the same :samp:`{prefix}` and :samp:`{num}`.

  If the string begins with :samp:`*`, then ``assemble_name`` will
  output the rest of the string unchanged.  It is often convenient for
  ``ASM_GENERATE_INTERNAL_LABEL`` to use :samp:`*` in this way.  If the
  string doesn't start with :samp:`*`, then ``ASM_OUTPUT_LABELREF`` gets
  to output the string, and may change it.  (Of course,
  ``ASM_OUTPUT_LABELREF`` is also part of your machine description, so
  you should know what it does on your machine.)

.. c:macro:: ASM_FORMAT_PRIVATE_NAME (outvar, name, number)

  A C expression to assign to :samp:`{outvar}` (which is a variable of type
  ``char *``) a newly allocated string made from the string
  :samp:`{name}` and the number :samp:`{number}`, with some suitable punctuation
  added.  Use ``alloca`` to get space for the string.

  The string will be used as an argument to ``ASM_OUTPUT_LABELREF`` to
  produce an assembler label for an internal static variable whose name is
  :samp:`{name}`.  Therefore, the string must be such as to result in valid
  assembler code.  The argument :samp:`{number}` is different each time this
  macro is executed; it prevents conflicts between similarly-named
  internal static variables in different scopes.

  Ideally this string should not be a valid C identifier, to prevent any
  conflict with the user's own symbols.  Most assemblers allow periods
  or percent signs in assembler symbols; putting at least one of these
  between the name and the number will suffice.

  If this macro is not defined, a default definition will be provided
  which is correct for most systems.

.. c:macro:: ASM_OUTPUT_DEF (stream, name, value)

  A C statement to output to the stdio stream :samp:`{stream}` assembler code
  which defines (equates) the symbol :samp:`{name}` to have the value :samp:`{value}`.

  .. index:: SET_ASM_OP

  If ``SET_ASM_OP`` is defined, a default definition is provided which is
  correct for most systems.

.. c:macro:: ASM_OUTPUT_DEF_FROM_DECLS (stream, decl_of_name, decl_of_value)

  A C statement to output to the stdio stream :samp:`{stream}` assembler code
  which defines (equates) the symbol whose tree node is :samp:`{decl_of_name}`
  to have the value of the tree node :samp:`{decl_of_value}`.  This macro will
  be used in preference to :samp:`ASM_OUTPUT_DEF` if it is defined and if
  the tree nodes are available.

  .. index:: SET_ASM_OP

  If ``SET_ASM_OP`` is defined, a default definition is provided which is
  correct for most systems.

.. c:macro:: TARGET_DEFERRED_OUTPUT_DEFS (decl_of_name, decl_of_value)

  A C statement that evaluates to true if the assembler code which defines
  (equates) the symbol whose tree node is :samp:`{decl_of_name}` to have the value
  of the tree node :samp:`{decl_of_value}` should be emitted near the end of the
  current compilation unit.  The default is to not defer output of defines.
  This macro affects defines output by :samp:`ASM_OUTPUT_DEF` and
  :samp:`ASM_OUTPUT_DEF_FROM_DECLS`.

.. c:macro:: ASM_OUTPUT_WEAK_ALIAS (stream, name, value)

  A C statement to output to the stdio stream :samp:`{stream}` assembler code
  which defines (equates) the weak symbol :samp:`{name}` to have the value
  :samp:`{value}`.  If :samp:`{value}` is ``NULL``, it defines :samp:`{name}` as
  an undefined weak symbol.

  Define this macro if the target only supports weak aliases; define
  ``ASM_OUTPUT_DEF`` instead if possible.

.. c:macro:: OBJC_GEN_METHOD_LABEL (buf, is_inst, class_name, cat_name, sel_name)

  Define this macro to override the default assembler names used for
  Objective-C methods.

  The default name is a unique method number followed by the name of the
  class (e.g. :samp:`_1_Foo`).  For methods in categories, the name of
  the category is also included in the assembler name (e.g.
  :samp:`_1_Foo_Bar`).

  These names are safe on most systems, but make debugging difficult since
  the method's selector is not present in the name.  Therefore, particular
  systems define other ways of computing names.

  :samp:`{buf}` is an expression of type ``char *`` which gives you a
  buffer in which to store the name; its length is as long as
  :samp:`{class_name}`, :samp:`{cat_name}` and :samp:`{sel_name}` put together, plus
  50 characters extra.

  The argument :samp:`{is_inst}` specifies whether the method is an instance
  method or a class method; :samp:`{class_name}` is the name of the class;
  :samp:`{cat_name}` is the name of the category (or ``NULL`` if the method is not
  in a category); and :samp:`{sel_name}` is the name of the selector.

  On systems where the assembler can handle quoted names, you can use this
  macro to provide more human-readable names.