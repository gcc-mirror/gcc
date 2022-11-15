..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: stringizing, # operator

.. _stringizing:

Stringizing
***********

Sometimes you may want to convert a macro argument into a string
constant.  Parameters are not replaced inside string constants, but you
can use the :samp:`#` preprocessing operator instead.  When a macro
parameter is used with a leading :samp:`#`, the preprocessor replaces it
with the literal text of the actual argument, converted to a string
constant.  Unlike normal parameter replacement, the argument is not
macro-expanded first.  This is called :dfn:`stringizing`.

There is no way to combine an argument with surrounding text and
stringize it all together.  Instead, you can write a series of adjacent
string constants and stringized arguments.  The preprocessor
replaces the stringized arguments with string constants.  The C
compiler then combines all the adjacent string constants into one
long string.

Here is an example of a macro definition that uses stringizing:

.. code-block::

  #define WARN_IF(EXP) \
  do { if (EXP) \
          fprintf (stderr, "Warning: " #EXP "\n"); } \
  while (0)
  WARN_IF (x == 0);
       → do { if (x == 0)
             fprintf (stderr, "Warning: " "x == 0" "\n"); } while (0);

The argument for ``EXP`` is substituted once, as-is, into the
``if`` statement, and once, stringized, into the argument to
``fprintf``.  If ``x`` were a macro, it would be expanded in the
``if`` statement, but not in the string.

The ``do`` and ``while (0)`` are a kludge to make it possible to
write ``WARN_IF (arg);``, which the resemblance of
``WARN_IF`` to a function would make C programmers want to do; see
:ref:`swallowing-the-semicolon`.

Stringizing in C involves more than putting double-quote characters
around the fragment.  The preprocessor backslash-escapes the quotes
surrounding embedded string constants, and all backslashes within string and
character constants, in order to get a valid C string constant with the
proper contents.  Thus, stringizing ``p = "foo\n";`` results in
``"p = \"foo\\n\";"``.  However, backslashes that are not inside string
or character constants are not duplicated: :samp:`\\n` by itself
stringizes to ``"\n"``.

All leading and trailing whitespace in text being stringized is
ignored.  Any sequence of whitespace in the middle of the text is
converted to a single space in the stringized result.  Comments are
replaced by whitespace long before stringizing happens, so they
never appear in stringized text.

There is no way to convert a macro argument into a character constant.

If you want to stringize the result of expansion of a macro argument,
you have to use two levels of macros.

.. code-block::

  #define xstr(s) str(s)
  #define str(s) #s
  #define foo 4
  str (foo)
       → "foo"
  xstr (foo)
       → xstr (4)
       → str (4)
       → "4"

``s`` is stringized when it is used in ``str``, so it is not
macro-expanded first.  But ``s`` is an ordinary argument to
``xstr``, so it is completely macro-expanded before ``xstr``
itself is expanded (see :ref:`argument-prescan`).  Therefore, by the time
``str`` gets to its argument, it has already been macro-expanded.
