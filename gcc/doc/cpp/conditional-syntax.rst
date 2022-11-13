..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: #if

.. _conditional-syntax:

Conditional Syntax
******************

A conditional in the C preprocessor begins with a :dfn:`conditional
directive`: :samp:`#if`, :samp:`#ifdef` or :samp:`#ifndef`.

.. toctree::
  :maxdepth: 2


.. index:: #ifdef, #endif

.. _ifdef:

Ifdef
^^^^^

The simplest sort of conditional is

.. code-block:: c++

  #ifdef MACRO

  controlled text

  #endif /* MACRO */

.. index:: conditional group

This block is called a :dfn:`conditional group`.  :samp:`{controlled text}`
will be included in the output of the preprocessor if and only if
:samp:`{MACRO}` is defined.  We say that the conditional :dfn:`succeeds` if
:samp:`{MACRO}` is defined, :dfn:`fails` if it is not.

The :samp:`{controlled text}` inside of a conditional can include
preprocessing directives.  They are executed only if the conditional
succeeds.  You can nest conditional groups inside other conditional
groups, but they must be completely nested.  In other words,
:samp:`#endif` always matches the nearest :samp:`#ifdef` (or
:samp:`#ifndef`, or :samp:`#if`).  Also, you cannot start a conditional
group in one file and end it in another.

Even if a conditional fails, the :samp:`{controlled text}` inside it is
still run through initial transformations and tokenization.  Therefore,
it must all be lexically valid C.  Normally the only way this matters is
that all comments and string literals inside a failing conditional group
must still be properly ended.

The comment following the :samp:`#endif` is not required, but it is a
good practice if there is a lot of :samp:`{controlled text}`, because it
helps people match the :samp:`#endif` to the corresponding :samp:`#ifdef`.
Older programs sometimes put :samp:`{MACRO}` directly after the
:samp:`#endif` without enclosing it in a comment.  This is invalid code
according to the C standard.  CPP accepts it with a warning.  It
never affects which :samp:`#ifndef` the :samp:`#endif` matches.

.. index:: #ifndef

Sometimes you wish to use some code if a macro is *not* defined.
You can do this by writing :samp:`#ifndef` instead of :samp:`#ifdef`.
One common use of :samp:`#ifndef` is to include code only the first
time a header file is included.  See :ref:`once-only-headers`.

Macro definitions can vary between compilations for several reasons.
Here are some samples.

* Some macros are predefined on each kind of machine
  (see :ref:`system-specific-predefined-macros`).  This allows you to provide
  code specially tuned for a particular machine.

* System header files define more macros, associated with the features
  they implement.  You can test these macros with conditionals to avoid
  using a system feature on a machine where it is not implemented.

* Macros can be defined or undefined with the :option:`-D` and :option:`-U`
  command-line options when you compile the program.  You can arrange to
  compile the same source file into two different programs by choosing a
  macro name to specify which program you want, writing conditionals to
  test whether or how this macro is defined, and then controlling the
  state of the macro with command-line options, perhaps set in the
  Makefile.  See :ref:`invocation`.

* Your program might have a special header file (often called
  :samp:`config.h`) that is adjusted when the program is compiled.  It can
  define or not define macros depending on the features of the system and
  the desired capabilities of the program.  The adjustment can be
  automated by a tool such as :command:`autoconf`, or done by hand.

.. _if:

If
^^

The :samp:`#if` directive allows you to test the value of an arithmetic
expression, rather than the mere existence of one macro.  Its syntax is

.. code-block:: c++

  #if expression

  controlled text

  #endif /* expression */

:samp:`{expression}` is a C expression of integer type, subject to stringent
restrictions.  It may contain

* Integer constants.

* Character constants, which are interpreted as they would be in normal
  code.

* Arithmetic operators for addition, subtraction, multiplication,
  division, bitwise operations, shifts, comparisons, and logical
  operations (``&&`` and ``||``).  The latter two obey the usual
  short-circuiting rules of standard C.

* Macros.  All macros in the expression are expanded before actual
  computation of the expression's value begins.

* Uses of the ``defined`` operator, which lets you check whether macros
  are defined in the middle of an :samp:`#if`.

* Identifiers that are not macros, which are all considered to be the
  number zero.  This allows you to write ``#if MACRO`` instead of
  ``#ifdef MACRO``, if you know that MACRO, when defined, will
  always have a nonzero value.  Function-like macros used without their
  function call parentheses are also treated as zero.

  In some contexts this shortcut is undesirable.  The :option:`-Wundef`
  option causes GCC to warn whenever it encounters an identifier which is
  not a macro in an :samp:`#if`.

The preprocessor does not know anything about types in the language.
Therefore, ``sizeof`` operators are not recognized in :samp:`#if`, and
neither are ``enum`` constants.  They will be taken as identifiers
which are not macros, and replaced by zero.  In the case of
``sizeof``, this is likely to cause the expression to be invalid.

The preprocessor calculates the value of :samp:`{expression}`.  It carries
out all calculations in the widest integer type known to the compiler;
on most machines supported by GCC this is 64 bits.  This is not the same
rule as the compiler uses to calculate the value of a constant
expression, and may give different results in some cases.  If the value
comes out to be nonzero, the :samp:`#if` succeeds and the :samp:`{controlled
text}` is included; otherwise it is skipped.

.. index:: defined

.. _defined:

Defined
^^^^^^^

The special operator ``defined`` is used in :samp:`#if` and
:samp:`#elif` expressions to test whether a certain name is defined as a
macro.  ``defined name`` and ``defined (name)`` are
both expressions whose value is 1 if :samp:`{name}` is defined as a macro at
the current point in the program, and 0 otherwise.  Thus,  ``#if
defined MACRO`` is precisely equivalent to ``#ifdef MACRO``.

``defined`` is useful when you wish to test more than one macro for
existence at once.  For example,

.. code-block:: c++

  #if defined (__vax__) || defined (__ns16000__)

would succeed if either of the names ``__vax__`` or
``__ns16000__`` is defined as a macro.

Conditionals written like this:

.. code-block:: c++

  #if defined BUFSIZE && BUFSIZE >= 1024

can generally be simplified to just ``#if BUFSIZE >= 1024``,
since if ``BUFSIZE`` is not defined, it will be interpreted as having
the value zero.

If the ``defined`` operator appears as a result of a macro expansion,
the C standard says the behavior is undefined.  GNU cpp treats it as a
genuine ``defined`` operator and evaluates it normally.  It will warn
wherever your code uses this feature if you use the command-line option
:option:`-Wpedantic`, since other compilers may handle it differently.  The
warning is also enabled by :option:`-Wextra`, and can also be enabled
individually with :option:`-Wexpansion-to-defined`.

.. index:: #else

.. _else:

Else
^^^^

The :samp:`#else` directive can be added to a conditional to provide
alternative text to be used if the condition fails.  This is what it
looks like:

.. code-block:: c++

  #if expression
  text-if-true
  #else /* Not expression */
  text-if-false
  #endif /* Not expression */

If :samp:`{expression}` is nonzero, the :samp:`{text-if-true}` is included and
the :samp:`{text-if-false}` is skipped.  If :samp:`{expression}` is zero, the
opposite happens.

You can use :samp:`#else` with :samp:`#ifdef` and :samp:`#ifndef`, too.

.. index:: #elif

.. _elif:

Elif
^^^^

One common case of nested conditionals is used to check for more than two
possible alternatives.  For example, you might have

.. code-block:: c++

  #if X == 1
  ...
  #else /* X != 1 */
  #if X == 2
  ...
  #else /* X != 2 */
  ...
  #endif /* X != 2 */
  #endif /* X != 1 */

Another conditional directive, :samp:`#elif`, allows this to be
abbreviated as follows:

.. code-block:: c++

  #if X == 1
  ...
  #elif X == 2
  ...
  #else /* X != 2 and X != 1*/
  ...
  #endif /* X != 2 and X != 1*/

:samp:`#elif` stands for 'else if'.  Like :samp:`#else`, it goes in the
middle of a conditional group and subdivides it; it does not require a
matching :samp:`#endif` of its own.  Like :samp:`#if`, the :samp:`#elif`
directive includes an expression to be tested.  The text following the
:samp:`#elif` is processed only if the original :samp:`#if`-condition
failed and the :samp:`#elif` condition succeeds.

More than one :samp:`#elif` can go in the same conditional group.  Then
the text after each :samp:`#elif` is processed only if the :samp:`#elif`
condition succeeds after the original :samp:`#if` and all previous
:samp:`#elif` directives within it have failed.

:samp:`#else` is allowed after any number of :samp:`#elif` directives, but
:samp:`#elif` may not follow :samp:`#else`.

.. index:: __has_attribute

__has_attribute
^^^^^^^^^^^^^^^

The special operator ``__has_attribute (operand)`` may be used
in :samp:`#if` and :samp:`#elif` expressions to test whether the attribute
referenced by its :samp:`{operand}` is recognized by GCC.  Using the operator
in other contexts is not valid.  In C code, if compiling for strict
conformance to standards before C2x, :samp:`{operand}` must be
a valid identifier.  Otherwise, :samp:`{operand}` may be optionally
introduced by the ``attribute-scope::`` prefix.
The :samp:`{attribute-scope}` prefix identifies the 'namespace' within
which the attribute is recognized.  The scope of GCC attributes is
:samp:`gnu` or :samp:`__gnu__`.  The ``__has_attribute`` operator by
itself, without any :samp:`{operand}` or parentheses, acts as a predefined
macro so that support for it can be tested in portable code.  Thus,
the recommended use of the operator is as follows:

.. code-block:: c++

  #if defined __has_attribute
  #  if __has_attribute (nonnull)
  #    define ATTR_NONNULL __attribute__ ((nonnull))
  #  endif
  #endif

The first :samp:`#if` test succeeds only when the operator is supported
by the version of GCC (or another compiler) being used.  Only when that
test succeeds is it valid to use ``__has_attribute`` as a preprocessor
operator.  As a result, combining the two tests into a single expression as
shown below would only be valid with a compiler that supports the operator
but not with others that don't.

.. code-block:: c++

  #if defined __has_attribute && __has_attribute (nonnull)   /* not portable */
  ...
  #endif

.. index:: __has_cpp_attribute

__has_cpp_attribute
^^^^^^^^^^^^^^^^^^^

The special operator ``__has_cpp_attribute (operand)`` may be used
in :samp:`#if` and :samp:`#elif` expressions in C++ code to test whether
the attribute referenced by its :samp:`{operand}` is recognized by GCC.
``__has_cpp_attribute (operand)`` is equivalent to
``__has_attribute (operand)`` except that when :samp:`{operand}`
designates a supported standard attribute it evaluates to an integer
constant of the form ``YYYYMM`` indicating the year and month when
the attribute was first introduced into the C++ standard.  For additional
information including the dates of the introduction of current standard
attributes, see `SD-6: SG10 Feature Test Recommendations <https://isocpp.org/std/standing-documents/sd-6-sg10-feature-test-recommendations/>`_.

.. index:: __has_c_attribute

__has_c_attribute
^^^^^^^^^^^^^^^^^

The special operator ``__has_c_attribute (operand)`` may be
used in :samp:`#if` and :samp:`#elif` expressions in C code to test
whether the attribute referenced by its :samp:`{operand}` is recognized by
GCC in attributes using the :samp:`[[]]` syntax.  GNU attributes must
be specified with the scope :samp:`gnu` or :samp:`__gnu__` with
``__has_c_attribute``.  When :samp:`{operand}` designates a supported
standard attribute it evaluates to an integer constant of the form
``YYYYMM`` indicating the year and month when the attribute was
first introduced into the C standard, or when the syntax of operands
to the attribute was extended in the C standard.

.. index:: __has_builtin

__has_builtin
^^^^^^^^^^^^^

The special operator ``__has_builtin (operand)`` may be used in
constant integer contexts and in preprocessor :samp:`#if` and :samp:`#elif`
expressions to test whether the symbol named by its :samp:`{operand}` is
recognized as a built-in function by GCC in the current language and
conformance mode.  It evaluates to a constant integer with a nonzero
value if the argument refers to such a function, and to zero otherwise.
The operator may also be used in preprocessor :samp:`#if` and :samp:`#elif`
expressions.  The ``__has_builtin`` operator by itself, without any
:samp:`{operand}` or parentheses, acts as a predefined macro so that support
for it can be tested in portable code.  Thus, the recommended use of
the operator is as follows:

.. code-block:: c++

  #if defined __has_builtin
  #  if __has_builtin (__builtin_object_size)
  #    define builtin_object_size(ptr) __builtin_object_size (ptr, 2)
  #  endif
  #endif
  #ifndef builtin_object_size
  #  define builtin_object_size(ptr)   ((size_t)-1)
  #endif

.. index:: __has_include

__has_include
^^^^^^^^^^^^^

The special operator ``__has_include (operand)`` may be used in
:samp:`#if` and :samp:`#elif` expressions to test whether the header referenced
by its :samp:`{operand}` can be included using the :samp:`#include` directive.  Using
the operator in other contexts is not valid.  The :samp:`{operand}` takes
the same form as the file in the :samp:`#include` directive (see :ref:`include-syntax`) and evaluates to a nonzero value if the header can be included and
to zero otherwise.  Note that that the ability to include a header doesn't
imply that the header doesn't contain invalid constructs or :samp:`#error`
directives that would cause the preprocessor to fail.

The ``__has_include`` operator by itself, without any :samp:`{operand}` or
parentheses, acts as a predefined macro so that support for it can be tested
in portable code.  Thus, the recommended use of the operator is as follows:

.. code-block:: c++

  #if defined __has_include
  #  if __has_include (<stdatomic.h>)
  #    include <stdatomic.h>
  #  endif
  #endif

The first :samp:`#if` test succeeds only when the operator is supported
by the version of GCC (or another compiler) being used.  Only when that
test succeeds is it valid to use ``__has_include`` as a preprocessor
operator.  As a result, combining the two tests into a single expression
as shown below would only be valid with a compiler that supports the operator
but not with others that don't.

.. code-block:: c++

  #if defined __has_include && __has_include ("header.h")   /* not portable */
  ...
  #endif