..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: inline functions, integrating function code, open coding, macros, inline alternative

.. _inline:

An Inline Function is As Fast As a Macro
****************************************

By declaring a function inline, you can direct GCC to make
calls to that function faster.  One way GCC can achieve this is to
integrate that function's code into the code for its callers.  This
makes execution faster by eliminating the function-call overhead; in
addition, if any of the actual argument values are constant, their
known values may permit simplifications at compile time so that not
all of the inline function's code needs to be included.  The effect on
code size is less predictable; object code may be larger or smaller
with function inlining, depending on the particular case.  You can
also direct GCC to try to integrate all 'simple enough' functions
into their callers with the option :option:`-finline-functions`.

GCC implements three different semantics of declaring a function
inline.  One is available with :option:`-std=gnu89` or
:option:`-fgnu89-inline` or when :gcc-attr:`gnu_inline` attribute is present
on all inline declarations, another when
:option:`-std=c99`,
:option:`-std=gnu99` or an option for a later C version is used
(without :option:`-fgnu89-inline`), and the third
is used when compiling C++.

To declare a function inline, use the ``inline`` keyword in its
declaration, like this:

.. code-block:: c++

  static inline int
  inc (int *a)
  {
    return (*a)++;
  }

If you are writing a header file to be included in ISO C90 programs, write
``__inline__`` instead of ``inline``.  See :ref:`alternate-keywords`.

The three types of inlining behave similarly in two important cases:
when the ``inline`` keyword is used on a ``static`` function,
like the example above, and when a function is first declared without
using the ``inline`` keyword and then is defined with
``inline``, like this:

.. code-block:: c++

  extern int inc (int *a);
  inline int
  inc (int *a)
  {
    return (*a)++;
  }

In both of these common cases, the program behaves the same as if you
had not used the ``inline`` keyword, except for its speed.

.. index:: inline functions, omission of, fkeep-inline-functions

When a function is both inline and ``static``, if all calls to the
function are integrated into the caller, and the function's address is
never used, then the function's own assembler code is never referenced.
In this case, GCC does not actually output assembler code for the
function, unless you specify the option :option:`-fkeep-inline-functions`.
If there is a nonintegrated call, then the function is compiled to
assembler code as usual.  The function must also be compiled as usual if
the program refers to its address, because that cannot be inlined.

.. index:: Winline

Note that certain usages in a function definition can make it unsuitable
for inline substitution.  Among these usages are: variadic functions,
use of ``alloca``, use of computed goto (see :ref:`labels-as-values`),
use of nonlocal goto, use of nested functions, use of ``setjmp``, use
of ``__builtin_longjmp`` and use of ``__builtin_return`` or
``__builtin_apply_args``.  Using :option:`-Winline` warns when a
function marked ``inline`` could not be substituted, and gives the
reason for the failure.

.. index:: automatic inline for C++ member fns, inline automatic for C++ member fns, member fns, automatically inline, C++ member fns, automatically inline, fno-default-inline

As required by ISO C++, GCC considers member functions defined within
the body of a class to be marked inline even if they are
not explicitly declared with the ``inline`` keyword.  You can
override this with :option:`-fno-default-inline` ; see :ref:`c++-dialect-options`.

GCC does not inline any functions when not optimizing unless you specify
the :samp:`always_inline` attribute for the function, like this:

.. code-block:: c++

  /* Prototype.  */
  inline void foo (const char) __attribute__((always_inline));

The remainder of this section is specific to GNU C90 inlining.

.. index:: non-static inline function

When an inline function is not ``static``, then the compiler must assume
that there may be calls from other source files; since a global symbol can
be defined only once in any program, the function must not be defined in
the other source files, so the calls therein cannot be integrated.
Therefore, a non- ``static`` inline function is always compiled on its
own in the usual fashion.

If you specify both ``inline`` and ``extern`` in the function
definition, then the definition is used only for inlining.  In no case
is the function compiled on its own, not even if you refer to its
address explicitly.  Such an address becomes an external reference, as
if you had only declared the function, and had not defined it.

This combination of ``inline`` and ``extern`` has almost the
effect of a macro.  The way to use it is to put a function definition in
a header file with these keywords, and put another copy of the
definition (lacking ``inline`` and ``extern``) in a library file.
The definition in the header file causes most calls to the function
to be inlined.  If any uses of the function remain, they refer to
the single copy in the library.