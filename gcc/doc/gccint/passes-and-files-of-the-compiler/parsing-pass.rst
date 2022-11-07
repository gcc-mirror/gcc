..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GENERIC, lang_hooks.parse_file

.. _parsing-pass:

Parsing pass
************

The language front end is invoked only once, via
``lang_hooks.parse_file``, to parse the entire input.  The language
front end may use any intermediate language representation deemed
appropriate.  The C front end uses GENERIC trees (see :ref:`generic`), plus
a double handful of language specific tree codes defined in
:samp:`c-common.def`.  The Fortran front end uses a completely different
private representation.

.. index:: GIMPLE, gimplification, gimplifier, language-independent intermediate representation, intermediate representation lowering, lowering, language-dependent intermediate representation

At some point the front end must translate the representation used in the
front end to a representation understood by the language-independent
portions of the compiler.  Current practice takes one of two forms.
The C front end manually invokes the gimplifier (see :ref:`gimple`) on each function,
and uses the gimplifier callbacks to convert the language-specific tree
nodes directly to GIMPLE before passing the function off to be compiled.
The Fortran front end converts from a private representation to GENERIC,
which is later lowered to GIMPLE when the function is compiled.  Which
route to choose probably depends on how well GENERIC (plus extensions)
can be made to match up with the source language and necessary parsing
data structures.

BUG: Gimplification must occur before nested function lowering,
and nested function lowering must be done by the front end before
passing the data off to cgraph.

.. todo:: Cgraph should control nested function lowering.  It would
  only be invoked when it is certain that the outer-most function
  is used.

.. todo:: Cgraph needs a gimplify_function callback.  It should be
  invoked when (1) it is certain that the function is used, (2)
  warning flags specified by the user require some amount of
  compilation in order to honor, (3) the language indicates that
  semantic analysis is not complete until gimplification occurs.
  Hum... this sounds overly complicated.  Perhaps we should just
  have the front end gimplify always; in most cases it's only one
  function call.

The front end needs to pass all function definitions and top level
declarations off to the middle-end so that they can be compiled and
emitted to the object file.  For a simple procedural language, it is
usually most convenient to do this as each top level declaration or
definition is seen.  There is also a distinction to be made between
generating functional code and generating complete debug information.
The only thing that is absolutely required for functional code is that
function and data *definitions* be passed to the middle-end.  For
complete debug information, function, data and type declarations
should all be passed as well.

.. index:: rest_of_decl_compilation, rest_of_type_compilation, cgraph_finalize_function

In any case, the front end needs each complete top-level function or
data declaration, and each data definition should be passed to
``rest_of_decl_compilation``.  Each complete type definition should
be passed to ``rest_of_type_compilation``.  Each function definition
should be passed to ``cgraph_finalize_function``.

.. todo:: I know rest_of_compilation currently has all sorts of
  RTL generation semantics.  I plan to move all code generation
  bits (both Tree and RTL) to compile_function.  Should we hide
  cgraph from the front ends and move back to rest_of_compilation
  as the official interface?  Possibly we should rename all three
  interfaces such that the names match in some meaningful way and
  that is more descriptive than "rest_of".

The middle-end will, at its option, emit the function and data
definitions immediately or queue them for later processing.