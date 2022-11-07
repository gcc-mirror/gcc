..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Overview
********

The lexer is contained in the file :samp:`lex.cc`.  It is a hand-coded
lexer, and not implemented as a state machine.  It can understand C, C++
and Objective-C source code, and has been extended to allow reasonably
successful preprocessing of assembly language.  The lexer does not make
an initial pass to strip out trigraphs and escaped newlines, but handles
them as they are encountered in a single pass of the input file.  It
returns preprocessing tokens individually, not a line at a time.

It is mostly transparent to users of the library, since the library's
interface for obtaining the next token, ``cpp_get_token``, takes care
of lexing new tokens, handling directives, and expanding macros as
necessary.  However, the lexer does expose some functionality so that
clients of the library can easily spell a given token, such as
``cpp_spell_token`` and ``cpp_token_len``.  These functions are
useful when generating diagnostics, and for emitting the preprocessed
output.