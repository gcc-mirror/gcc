..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Macro expansion overview
************************

The preprocessor maintains a :dfn:`context stack`, implemented as a
linked list of ``cpp_context`` structures, which together represent
the macro expansion state at any one time.  The ``struct
cpp_reader`` member variable ``context`` points to the current top
of this stack.  The top normally holds the unexpanded replacement list
of the innermost macro under expansion, except when cpplib is about to
pre-expand an argument, in which case it holds that argument's
unexpanded tokens.

When there are no macros under expansion, cpplib is in :dfn:`base
context`.  All contexts other than the base context contain a
contiguous list of tokens delimited by a starting and ending token.
When not in base context, cpplib obtains the next token from the list
of the top context.  If there are no tokens left in the list, it pops
that context off the stack, and subsequent ones if necessary, until an
unexhausted context is found or it returns to base context.  In base
context, cpplib reads tokens directly from the lexer.

If it encounters an identifier that is both a macro and enabled for
expansion, cpplib prepares to push a new context for that macro on the
stack by calling the routine ``enter_macro_context``.  When this
routine returns, the new context will contain the unexpanded tokens of
the replacement list of that macro.  In the case of function-like
macros, ``enter_macro_context`` also replaces any parameters in the
replacement list, stored as ``CPP_MACRO_ARG`` tokens, with the
appropriate macro argument.  If the standard requires that the
parameter be replaced with its expanded argument, the argument will
have been fully macro expanded first.

``enter_macro_context`` also handles special macros like
``__LINE__``.  Although these macros expand to a single token which
cannot contain any further macros, for reasons of token spacing
(see :ref:`token-spacing`) and simplicity of implementation, cpplib
handles these special macros by pushing a context containing just that
one token.

The final thing that ``enter_macro_context`` does before returning
is to mark the macro disabled for expansion (except for special macros
like ``__TIME__``).  The macro is re-enabled when its context is
later popped from the context stack, as described above.  This strict
ordering ensures that a macro is disabled whilst its expansion is
being scanned, but that it is *not* disabled whilst any arguments
to it are being expanded.
