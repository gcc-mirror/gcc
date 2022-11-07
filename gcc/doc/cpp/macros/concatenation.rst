..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: concatenation, token pasting, token concatenation, ## operator

.. _concatenation:

Concatenation
*************

It is often useful to merge two tokens into one while expanding macros.
This is called :dfn:`token pasting` or :dfn:`token concatenation`.  The
:samp:`##` preprocessing operator performs token pasting.  When a macro
is expanded, the two tokens on either side of each :samp:`##` operator
are combined into a single token, which then replaces the :samp:`##` and
the two original tokens in the macro expansion.  Usually both will be
identifiers, or one will be an identifier and the other a preprocessing
number.  When pasted, they make a longer identifier.  This isn't the
only valid case.  It is also possible to concatenate two numbers (or a
number and a name, such as ``1.5`` and ``e3``) into a number.
Also, multi-character operators such as ``+=`` can be formed by
token pasting.

However, two tokens that don't together form a valid token cannot be
pasted together.  For example, you cannot concatenate ``x`` with
``+`` in either order.  If you try, the preprocessor issues a warning
and emits the two tokens.  Whether it puts white space between the
tokens is undefined.  It is common to find unnecessary uses of :samp:`##`
in complex macros.  If you get this warning, it is likely that you can
simply remove the :samp:`##`.

Both the tokens combined by :samp:`##` could come from the macro body,
but you could just as well write them as one token in the first place.
Token pasting is most useful when one or both of the tokens comes from a
macro argument.  If either of the tokens next to an :samp:`##` is a
parameter name, it is replaced by its actual argument before :samp:`##`
executes.  As with stringizing, the actual argument is not
macro-expanded first.  If the argument is empty, that :samp:`##` has no
effect.

Keep in mind that the C preprocessor converts comments to whitespace
before macros are even considered.  Therefore, you cannot create a
comment by concatenating :samp:`/` and :samp:`*`.  You can put as much
whitespace between :samp:`##` and its operands as you like, including
comments, and you can put comments in arguments that will be
concatenated.  However, it is an error if :samp:`##` appears at either
end of a macro body.

Consider a C program that interprets named commands.  There probably
needs to be a table of commands, perhaps an array of structures declared
as follows:

.. code-block:: c++

  struct command
  {
    char *name;
    void (*function) (void);
  };

  struct command commands[] =
  {
    { "quit", quit_command },
    { "help", help_command },
    ...
  };

It would be cleaner not to have to give each command name twice, once in
the string constant and once in the function name.  A macro which takes the
name of a command as an argument can make this unnecessary.  The string
constant can be created with stringizing, and the function name by
concatenating the argument with :samp:`_command`.  Here is how it is done:

.. code-block:: c++

  #define COMMAND(NAME)  { #NAME, NAME ## _command }

  struct command commands[] =
  {
    COMMAND (quit),
    COMMAND (help),
    ...
  };