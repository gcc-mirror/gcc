..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: computed includes, macros in include

.. _computed-includes:

Computed Includes
*****************

Sometimes it is necessary to select one of several different header
files to be included into your program.  They might specify
configuration parameters to be used on different sorts of operating
systems, for instance.  You could do this with a series of conditionals,

.. code-block:: c++

  #if SYSTEM_1
  # include "system_1.h"
  #elif SYSTEM_2
  # include "system_2.h"
  #elif SYSTEM_3
  ...
  #endif

That rapidly becomes tedious.  Instead, the preprocessor offers the
ability to use a macro for the header name.  This is called a
:dfn:`computed include`.  Instead of writing a header name as the direct
argument of :samp:`#include`, you simply put a macro name there instead:

.. code-block:: c++

  #define SYSTEM_H "system_1.h"
  ...
  #include SYSTEM_H

``SYSTEM_H`` will be expanded, and the preprocessor will look for
:samp:`system_1.h` as if the :samp:`#include` had been written that way
originally.  ``SYSTEM_H`` could be defined by your Makefile with a
:option:`-D` option.

You must be careful when you define the macro.  :samp:`#define` saves
tokens, not text.  The preprocessor has no way of knowing that the macro
will be used as the argument of :samp:`#include`, so it generates
ordinary tokens, not a header name.  This is unlikely to cause problems
if you use double-quote includes, which are close enough to string
constants.  If you use angle brackets, however, you may have trouble.

The syntax of a computed include is actually a bit more general than the
above.  If the first non-whitespace character after :samp:`#include` is
not :samp:`"` or :samp:`<`, then the entire line is macro-expanded
like running text would be.

If the line expands to a single string constant, the contents of that
string constant are the file to be included.  CPP does not re-examine the
string for embedded quotes, but neither does it process backslash
escapes in the string.  Therefore

.. code-block:: c++

  #define HEADER "a\"b"
  #include HEADER

looks for a file named :samp:`a\\"b`.  CPP searches for the file according
to the rules for double-quoted includes.

If the line expands to a token stream beginning with a :samp:`<` token
and including a :samp:`>` token, then the tokens between the :samp:`<` and
the first :samp:`>` are combined to form the filename to be included.
Any whitespace between tokens is reduced to a single space; then any
space after the initial :samp:`<` is retained, but a trailing space
before the closing :samp:`>` is ignored.  CPP searches for the file
according to the rules for angle-bracket includes.

In either case, if there are any tokens on the line after the file name,
an error occurs and the directive is not processed.  It is also an error
if the result of expansion does not match either of the two expected
forms.

These rules are implementation-defined behavior according to the C
standard.  To minimize the risk of different compilers interpreting your
computed includes differently, we recommend you use only a single
object-like macro which expands to a string constant.  This will also
minimize confusion for people reading your program.