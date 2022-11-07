..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: __func__ identifier, __FUNCTION__ identifier, __PRETTY_FUNCTION__ identifier

.. _function-names:

Function Names as Strings
*************************

GCC provides three magic constants that hold the name of the current
function as a string.  In C++11 and later modes, all three are treated
as constant expressions and can be used in ``constexpr`` constexts.
The first of these constants is ``__func__``, which is part of
the C99 standard:

The identifier ``__func__`` is implicitly declared by the translator
as if, immediately following the opening brace of each function
definition, the declaration

.. code-block:: c++

  static const char __func__[] = "function-name";

appeared, where function-name is the name of the lexically-enclosing
function.  This name is the unadorned name of the function.  As an
extension, at file (or, in C++, namespace scope), ``__func__``
evaluates to the empty string.

``__FUNCTION__`` is another name for ``__func__``, provided for
backward compatibility with old versions of GCC.

In C, ``__PRETTY_FUNCTION__`` is yet another name for
``__func__``, except that at file scope (or, in C++, namespace scope),
it evaluates to the string ``"top level"``.  In addition, in C++,
``__PRETTY_FUNCTION__`` contains the signature of the function as
well as its bare name.  For example, this program:

.. code-block:: c++

  extern "C" int printf (const char *, ...);

  class a {
   public:
    void sub (int i)
      {
        printf ("__FUNCTION__ = %s\n", __FUNCTION__);
        printf ("__PRETTY_FUNCTION__ = %s\n", __PRETTY_FUNCTION__);
      }
  };

  int
  main (void)
  {
    a ax;
    ax.sub (0);
    return 0;
  }

gives this output:

.. code-block:: c++

  __FUNCTION__ = sub
  __PRETTY_FUNCTION__ = void a::sub(int)

These identifiers are variables, not preprocessor macros, and may not
be used to initialize ``char`` arrays or be concatenated with string
literals.