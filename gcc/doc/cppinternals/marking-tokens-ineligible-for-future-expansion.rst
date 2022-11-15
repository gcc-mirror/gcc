..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Marking tokens ineligible for future expansion
**********************************************

As discussed above, cpplib needs a way of marking tokens as
unexpandable.  Since the tokens cpplib handles are read-only once they
have been lexed, it instead makes a copy of the token and adds the
flag ``NO_EXPAND`` to the copy.

For efficiency and to simplify memory management by avoiding having to
remember to free these tokens, they are allocated as temporary tokens
from the lexer's current token run (see :ref:`lexing-a-line`) using the
function ``_cpp_temp_token``.  The tokens are then re-used once the
current line of tokens has been read in.

This might sound unsafe.  However, tokens runs are not re-used at the
end of a line if it happens to be in the middle of a macro argument
list, and cpplib only wants to back-up more than one lexer token in
situations where no macro expansion is involved, so the optimization
is safe.
