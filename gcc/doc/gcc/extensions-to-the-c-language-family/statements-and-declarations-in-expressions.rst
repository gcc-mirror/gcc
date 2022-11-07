..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: statements inside expressions, declarations inside expressions, expressions containing statements, macros, statements in expressions

.. _statement-exprs:

Statements and Declarations in Expressions
******************************************

.. the above section title wrapped and causes an underfull hbox.. i
   changed it from "within" to "in". -mew 4feb93

A compound statement enclosed in parentheses may appear as an expression
in GNU C.  This allows you to use loops, switches, and local variables
within an expression.

Recall that a compound statement is a sequence of statements surrounded
by braces; in this construct, parentheses go around the braces.  For
example:

.. code-block:: c++

  ({ int y = foo (); int z;
     if (y > 0) z = y;
     else z = - y;
     z; })

is a valid (though slightly more complex than necessary) expression
for the absolute value of ``foo ()``.

The last thing in the compound statement should be an expression
followed by a semicolon; the value of this subexpression serves as the
value of the entire construct.  (If you use some other kind of statement
last within the braces, the construct has type ``void``, and thus
effectively no value.)

This feature is especially useful in making macro definitions 'safe' (so
that they evaluate each operand exactly once).  For example, the
'maximum' function is commonly defined as a macro in standard C as
follows:

.. code-block:: c++

  #define max(a,b) ((a) > (b) ? (a) : (b))

.. index:: side effects, macro argument

But this definition computes either :samp:`{a}` or :samp:`{b}` twice, with bad
results if the operand has side effects.  In GNU C, if you know the
type of the operands (here taken as ``int``), you can avoid this
problem by defining the macro as follows:

.. code-block:: c++

  #define maxint(a,b) \
    ({int _a = (a), _b = (b); _a > _b ? _a : _b; })

Note that introducing variable declarations (as we do in ``maxint``) can
cause variable shadowing, so while this example using the ``max`` macro
produces correct results:

.. code-block:: c++

  int _a = 1, _b = 2, c;
  c = max (_a, _b);

this example using maxint will not:

.. code-block:: c++

  int _a = 1, _b = 2, c;
  c = maxint (_a, _b);

This problem may for instance occur when we use this pattern recursively, like
so:

.. code-block:: c++

  #define maxint3(a, b, c) \
    ({int _a = (a), _b = (b), _c = (c); maxint (maxint (_a, _b), _c); })

Embedded statements are not allowed in constant expressions, such as
the value of an enumeration constant, the width of a bit-field, or
the initial value of a static variable.

If you don't know the type of the operand, you can still do this, but you
must use ``typeof`` or ``__auto_type`` (see :ref:`typeof`).

In G++, the result value of a statement expression undergoes array and
function pointer decay, and is returned by value to the enclosing
expression.  For instance, if ``A`` is a class, then

.. code-block:: c++

          A a;

          ({a;}).Foo ()

constructs a temporary ``A`` object to hold the result of the
statement expression, and that is used to invoke ``Foo``.
Therefore the ``this`` pointer observed by ``Foo`` is not the
address of ``a``.

In a statement expression, any temporaries created within a statement
are destroyed at that statement's end.  This makes statement
expressions inside macros slightly different from function calls.  In
the latter case temporaries introduced during argument evaluation are
destroyed at the end of the statement that includes the function
call.  In the statement expression case they are destroyed during
the statement expression.  For instance,

.. code-block:: c++

  #define macro(a)  ({__typeof__(a) b = (a); b + 3; })
  template<typename T> T function(T a) { T b = a; return b + 3; }

  void foo ()
  {
    macro (X ());
    function (X ());
  }

has different places where temporaries are destroyed.  For the
``macro`` case, the temporary ``X`` is destroyed just after
the initialization of ``b``.  In the ``function`` case that
temporary is destroyed when the function returns.

These considerations mean that it is probably a bad idea to use
statement expressions of this form in header files that are designed to
work with C++.  (Note that some versions of the GNU C Library contained
header files using statement expressions that lead to precisely this
bug.)

Jumping into a statement expression with ``goto`` or using a
``switch`` statement outside the statement expression with a
``case`` or ``default`` label inside the statement expression is
not permitted.  Jumping into a statement expression with a computed
``goto`` (see :ref:`labels-as-values`) has undefined behavior.
Jumping out of a statement expression is permitted, but if the
statement expression is part of a larger expression then it is
unspecified which other subexpressions of that expression have been
evaluated except where the language definition requires certain
subexpressions to be evaluated before or after the statement
expression.  A ``break`` or ``continue`` statement inside of
a statement expression used in ``while``, ``do`` or ``for``
loop or ``switch`` statement condition
or ``for`` statement init or increment expressions jumps to an
outer loop or ``switch`` statement if any (otherwise it is an error),
rather than to the loop or ``switch`` statement in whose condition
or init or increment expression it appears.
In any case, as with a function call, the evaluation of a
statement expression is not interleaved with the evaluation of other
parts of the containing expression.  For example,

.. code-block:: c++

    foo (), (({ bar1 (); goto a; 0; }) + bar2 ()), baz();

calls ``foo`` and ``bar1`` and does not call ``baz`` but
may or may not call ``bar2``.  If ``bar2`` is called, it is
called after ``foo`` and before ``bar1``.