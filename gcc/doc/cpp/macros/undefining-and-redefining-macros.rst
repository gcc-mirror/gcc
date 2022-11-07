..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: undefining macros, redefining macros, #undef

.. _undefining-and-redefining-macros:

Undefining and Redefining Macros
********************************

If a macro ceases to be useful, it may be :dfn:`undefined` with the
:samp:`#undef` directive.  :samp:`#undef` takes a single argument, the
name of the macro to undefine.  You use the bare macro name, even if the
macro is function-like.  It is an error if anything appears on the line
after the macro name.  :samp:`#undef` has no effect if the name is not a
macro.

.. code-block::

  #define FOO 4
  x = FOO;        → x = 4;
  #undef FOO
  x = FOO;        → x = FOO;

Once a macro has been undefined, that identifier may be :dfn:`redefined`
as a macro by a subsequent :samp:`#define` directive.  The new definition
need not have any resemblance to the old definition.

However, if an identifier which is currently a macro is redefined, then
the new definition must be :dfn:`effectively the same` as the old one.
Two macro definitions are effectively the same if:

* Both are the same type of macro (object- or function-like).

* All the tokens of the replacement list are the same.

* If there are any parameters, they are the same.

* Whitespace appears in the same places in both.  It need not be
  exactly the same amount of whitespace, though.  Remember that comments
  count as whitespace.

These definitions are effectively the same:

.. code-block:: c++

  #define FOUR (2 + 2)
  #define FOUR         (2    +    2)
  #define FOUR (2 /* two */ + 2)

but these are not:

.. code-block:: c++

  #define FOUR (2 + 2)
  #define FOUR ( 2+2 )
  #define FOUR (2 * 2)
  #define FOUR(score,and,seven,years,ago) (2 + 2)

If a macro is redefined with a definition that is not effectively the
same as the old one, the preprocessor issues a warning and changes the
macro to use the new definition.  If the new definition is effectively
the same, the redefinition is silently ignored.  This allows, for
instance, two different headers to define a common macro.  The
preprocessor will only complain if the definitions do not match.