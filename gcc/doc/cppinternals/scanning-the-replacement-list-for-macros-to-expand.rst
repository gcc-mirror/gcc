..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Scanning the replacement list for macros to expand
**************************************************

The C standard states that, after any parameters have been replaced
with their possibly-expanded arguments, the replacement list is
scanned for nested macros.  Further, any identifiers in the
replacement list that are not expanded during this scan are never
again eligible for expansion in the future, if the reason they were
not expanded is that the macro in question was disabled.

Clearly this latter condition can only apply to tokens resulting from
argument pre-expansion.  Other tokens never have an opportunity to be
re-tested for expansion.  It is possible for identifiers that are
function-like macros to not expand initially but to expand during a
later scan.  This occurs when the identifier is the last token of an
argument (and therefore originally followed by a comma or a closing
parenthesis in its macro's argument list), and when it replaces its
parameter in the macro's replacement list, the subsequent token
happens to be an opening parenthesis (itself possibly the first token
of an argument).

It is important to note that when cpplib reads the last token of a
given context, that context still remains on the stack.  Only when
looking for the *next* token do we pop it off the stack and drop
to a lower context.  This makes backing up by one token easy, but more
importantly ensures that the macro corresponding to the current
context is still disabled when we are considering the last token of
its replacement list for expansion (or indeed expanding it).  As an
example, which illustrates many of the points above, consider

.. code-block:: c++

  #define foo(x) bar x
  foo(foo) (2)

which fully expands to :samp:`bar foo (2)`.  During pre-expansion
of the argument, :samp:`foo` does not expand even though the macro is
enabled, since it has no following parenthesis [pre-expansion of an
argument only uses tokens from that argument; it cannot take tokens
from whatever follows the macro invocation].  This still leaves the
argument token :samp:`foo` eligible for future expansion.  Then, when
re-scanning after argument replacement, the token :samp:`foo` is
rejected for expansion, and marked ineligible for future expansion,
since the macro is now disabled.  It is disabled because the
replacement list :samp:`bar foo` of the macro is still on the context
stack.

If instead the algorithm looked for an opening parenthesis first and
then tested whether the macro were disabled it would be subtly wrong.
In the example above, the replacement list of :samp:`foo` would be
popped in the process of finding the parenthesis, re-enabling
:samp:`foo` and expanding it a second time.