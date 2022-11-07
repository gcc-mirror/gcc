The Multiple-Include Optimization
=================================

.. index:: guard macros, controlling macros, multiple-include optimization

Header files are often of the form

.. code-block:: c++

  #ifndef FOO
  #define FOO
  ...
  #endif

to prevent the compiler from processing them more than once.  The
preprocessor notices such header files, so that if the header file
appears in a subsequent ``#include`` directive and ``FOO`` is
defined, then it is ignored and it doesn't preprocess or even re-open
the file a second time.  This is referred to as the :dfn:`multiple
include optimization`.

Under what circumstances is such an optimization valid?  If the file
were included a second time, it can only be optimized away if that
inclusion would result in no tokens to return, and no relevant
directives to process.  Therefore the current implementation imposes
requirements and makes some allowances as follows:

* There must be no tokens outside the controlling ``#if`` - ``#endif``
  pair, but whitespace and comments are permitted.

* There must be no directives outside the controlling directive pair, but
  the :dfn:`null directive` (a line containing nothing other than a single
  :samp:`#` and possibly whitespace) is permitted.

* The opening directive must be of the form

  .. code-block:: c++

    #ifndef FOO

  or

  .. code-block:: c++

    #if !defined FOO     [equivalently, #if !defined(FOO)]

* In the second form above, the tokens forming the ``#if`` expression
  must have come directly from the source file---no macro expansion must
  have been involved.  This is because macro definitions can change, and
  tracking whether or not a relevant change has been made is not worth the
  implementation cost.

* There can be no ``#else`` or ``#elif`` directives at the outer
  conditional block level, because they would probably contain something
  of interest to a subsequent pass.

First, when pushing a new file on the buffer stack,
``_stack_include_file`` sets the controlling macro ``mi_cmacro`` to
``NULL``, and sets ``mi_valid`` to ``true``.  This indicates
that the preprocessor has not yet encountered anything that would
invalidate the multiple-include optimization.  As described in the next
few paragraphs, these two variables having these values effectively
indicates top-of-file.

When about to return a token that is not part of a directive,
``_cpp_lex_token`` sets ``mi_valid`` to ``false``.  This
enforces the constraint that tokens outside the controlling conditional
block invalidate the optimization.

The ``do_if``, when appropriate, and ``do_ifndef`` directive
handlers pass the controlling macro to the function
``push_conditional``.  cpplib maintains a stack of nested conditional
blocks, and after processing every opening conditional this function
pushes an ``if_stack`` structure onto the stack.  In this structure
it records the controlling macro for the block, provided there is one
and we're at top-of-file (as described above).  If an ``#elif`` or
``#else`` directive is encountered, the controlling macro for that
block is cleared to ``NULL``.  Otherwise, it survives until the
``#endif`` closing the block, upon which ``do_endif`` sets
``mi_valid`` to true and stores the controlling macro in
``mi_cmacro``.

``_cpp_handle_directive`` clears ``mi_valid`` when processing any
directive other than an opening conditional and the null directive.
With this, and requiring top-of-file to record a controlling macro, and
no ``#else`` or ``#elif`` for it to survive and be copied to
``mi_cmacro`` by ``do_endif``, we have enforced the absence of
directives outside the main conditional block for the optimization to be
on.

Note that whilst we are inside the conditional block, ``mi_valid`` is
likely to be reset to ``false``, but this does not matter since
the closing ``#endif`` restores it to ``true`` if appropriate.

Finally, since ``_cpp_lex_direct`` pops the file off the buffer stack
at ``EOF`` without returning a token, if the ``#endif`` directive
was not followed by any tokens, ``mi_valid`` is ``true`` and
``_cpp_pop_file_buffer`` remembers the controlling macro associated
with the file.  Subsequent calls to ``stack_include_file`` result in
no buffer being pushed if the controlling macro is defined, effecting
the optimization.

A quick word on how we handle the

.. code-block:: c++

  #if !defined FOO

case.  ``_cpp_parse_expr`` and ``parse_defined`` take steps to see
whether the three stages :samp:`!`, :samp:`defined-expression` and
:samp:`end-of-directive` occur in order in a ``#if`` expression.  If
so, they return the guard macro to ``do_if`` in the variable
``mi_ind_cmacro``, and otherwise set it to ``NULL``.
``enter_macro_context`` sets ``mi_valid`` to false, so if a macro
was expanded whilst parsing any part of the expression, then the
top-of-file test in ``push_conditional`` fails and the optimization
is turned off.