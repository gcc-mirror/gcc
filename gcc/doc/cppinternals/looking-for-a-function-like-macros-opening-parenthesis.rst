..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Looking for a function-like macro's opening parenthesis
*******************************************************

Function-like macros only expand when immediately followed by a
parenthesis.  To do this cpplib needs to temporarily disable macros
and read the next token.  Unfortunately, because of spacing issues
(see :ref:`token-spacing`), there can be fake padding tokens in-between,
and if the next real token is not a parenthesis cpplib needs to be
able to back up that one token as well as retain the information in
any intervening padding tokens.

Backing up more than one token when macros are involved is not
permitted by cpplib, because in general it might involve issues like
restoring popped contexts onto the context stack, which are too hard.
Instead, searching for the parenthesis is handled by a special
function, ``funlike_invocation_p``, which remembers padding
information as it reads tokens.  If the next real token is not an
opening parenthesis, it backs up that one token, and then pushes an
extra context just containing the padding information if necessary.