..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Variable Location Debug Information in RTL

.. _debug-information:

Variable Location Debug Information in RTL
******************************************

Variable tracking relies on ``MEM_EXPR`` and ``REG_EXPR``
annotations to determine what user variables memory and register
references refer to.

Variable tracking at assignments uses these notes only when they refer
to variables that live at fixed locations (e.g., addressable
variables, global non-automatic variables).  For variables whose
location may vary, it relies on the following types of notes.

.. index:: var_location

:samp:`(var_location:{mode} {var} {exp} {stat})`
  Binds variable ``var``, a tree, to value :samp:`{exp}`, an RTL
  expression.  It appears only in ``NOTE_INSN_VAR_LOCATION`` and
  ``DEBUG_INSN`` s, with slightly different meanings.  :samp:`{mode}`, if
  present, represents the mode of :samp:`{exp}`, which is useful if it is a
  modeless expression.  :samp:`{stat}` is only meaningful in notes,
  indicating whether the variable is known to be initialized or
  uninitialized.

  .. index:: debug_expr

:samp:`(debug_expr:{mode} {decl})`
  Stands for the value bound to the ``DEBUG_EXPR_DECL`` :samp:`{decl}`,
  that points back to it, within value expressions in
  ``VAR_LOCATION`` nodes.

  .. index:: debug_implicit_ptr

:samp:`(debug_implicit_ptr:{mode} {decl})`
  Stands for the location of a :samp:`{decl}` that is no longer addressable.

  .. index:: entry_value

:samp:`(entry_value:{mode} {decl})`
  Stands for the value a :samp:`{decl}` had at the entry point of the
  containing function.

  .. index:: debug_parameter_ref

:samp:`(debug_parameter_ref:{mode} {decl})`
  Refers to a parameter that was completely optimized out.

  .. index:: debug_marker

:samp:`(debug_marker:{mode})`
  Marks a program location.  With ``VOIDmode``, it stands for the
  beginning of a statement, a recommended inspection point logically after
  all prior side effects, and before any subsequent side effects.  With
  ``BLKmode``, it indicates an inline entry point: the lexical block
  encoded in the ``INSN_LOCATION`` is the enclosing block that encloses
  the inlined function.
