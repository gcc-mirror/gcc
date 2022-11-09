..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: function prototype declarations, old-style function definitions, promotion of formal parameters

.. _function-prototypes:

Prototypes and Old-Style Function Definitions
*********************************************

GNU C extends ISO C to allow a function prototype to override a later
old-style non-prototype definition.  Consider the following example:

.. code-block:: c++

  /* Use prototypes unless the compiler is old-fashioned.  */
  #ifdef __STDC__
  #define P(x) x
  #else
  #define P(x) ()
  #endif

  /* Prototype function declaration.  */
  int isroot P((uid_t));

  /* Old-style function definition.  */
  int
  isroot (x)   /* ??? lossage here ??? */
       uid_t x;
  {
    return x == 0;
  }

Suppose the type ``uid_t`` happens to be ``short``.  ISO C does
not allow this example, because subword arguments in old-style
non-prototype definitions are promoted.  Therefore in this example the
function definition's argument is really an ``int``, which does not
match the prototype argument type of ``short``.

This restriction of ISO C makes it hard to write code that is portable
to traditional C compilers, because the programmer does not know
whether the ``uid_t`` type is ``short``, ``int``, or
``long``.  Therefore, in cases like these GNU C allows a prototype
to override a later old-style definition.  More precisely, in GNU C, a
function prototype argument type overrides the argument type specified
by a later old-style definition if the former type is the same as the
latter type before promotion.  Thus in GNU C the above example is
equivalent to the following:

.. code-block:: c++

  int isroot (uid_t);

  int
  isroot (uid_t x)
  {
    return x == 0;
  }

GNU C++ does not support old-style function definitions, so this
extension is irrelevant.
