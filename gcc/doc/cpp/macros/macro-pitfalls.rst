..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: problems with macros, pitfalls of macros

.. _macro-pitfalls:

Macro Pitfalls
**************

In this section we describe some special rules that apply to macros and
macro expansion, and point out certain cases in which the rules have
counter-intuitive consequences that you must watch out for.

.. toctree::
  :maxdepth: 2


.. _misnesting:

Misnesting
^^^^^^^^^^

When a macro is called with arguments, the arguments are substituted
into the macro body and the result is checked, together with the rest of
the input file, for more macro calls.  It is possible to piece together
a macro call coming partially from the macro body and partially from the
arguments.  For example,

.. code-block::

  #define twice(x) (2*(x))
  #define call_with_1(x) x(1)
  call_with_1 (twice)
       → twice(1)
       → (2*(1))

Macro definitions do not have to have balanced parentheses.  By writing
an unbalanced open parenthesis in a macro body, it is possible to create
a macro call that begins inside the macro body but ends outside of it.
For example,

.. code-block::

  #define strange(file) fprintf (file, "%s %d",
  ...
  strange(stderr) p, 35)
       → fprintf (stderr, "%s %d", p, 35)

The ability to piece together a macro call can be useful, but the use of
unbalanced open parentheses in a macro body is just confusing, and
should be avoided.

.. index:: parentheses in macro bodies

.. _operator-precedence-problems:

Operator Precedence Problems
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You may have noticed that in most of the macro definition examples shown
above, each occurrence of a macro argument name had parentheses around
it.  In addition, another pair of parentheses usually surround the
entire macro definition.  Here is why it is best to write macros that
way.

Suppose you define a macro as follows,

.. code-block:: c++

  #define ceil_div(x, y) (x + y - 1) / y

whose purpose is to divide, rounding up.  (One use for this operation is
to compute how many ``int`` objects are needed to hold a certain
number of ``char`` objects.)  Then suppose it is used as follows:

.. code-block::

  a = ceil_div (b & c, sizeof (int));
       → a = (b & c + sizeof (int) - 1) / sizeof (int);

This does not do what is intended.  The operator-precedence rules of
C make it equivalent to this:

.. code-block:: c++

  a = (b & (c + sizeof (int) - 1)) / sizeof (int);

What we want is this:

.. code-block:: c++

  a = ((b & c) + sizeof (int) - 1)) / sizeof (int);

Defining the macro as

.. code-block:: c++

  #define ceil_div(x, y) ((x) + (y) - 1) / (y)

provides the desired result.

Unintended grouping can result in another way.  Consider ``sizeof
ceil_div(1, 2)``.  That has the appearance of a C expression that would
compute the size of the type of ``ceil_div (1, 2)``, but in fact it
means something very different.  Here is what it expands to:

.. code-block:: c++

  sizeof ((1) + (2) - 1) / (2)

This would take the size of an integer and divide it by two.  The
precedence rules have put the division outside the ``sizeof`` when it
was intended to be inside.

Parentheses around the entire macro definition prevent such problems.
Here, then, is the recommended way to define ``ceil_div`` :

.. code-block:: c++

  #define ceil_div(x, y) (((x) + (y) - 1) / (y))

.. index:: semicolons (after macro calls)

.. _swallowing-the-semicolon:

Swallowing the Semicolon
^^^^^^^^^^^^^^^^^^^^^^^^

Often it is desirable to define a macro that expands into a compound
statement.  Consider, for example, the following macro, that advances a
pointer (the argument ``p`` says where to find it) across whitespace
characters:

.. code-block:: c++

  #define SKIP_SPACES(p, limit)  \
  { char *lim = (limit);         \
    while (p < lim) {            \
      if (*p++ != ' ') {         \
        p--; break; }}}

Here backslash-newline is used to split the macro definition, which must
be a single logical line, so that it resembles the way such code would
be laid out if not part of a macro definition.

A call to this macro might be ``SKIP_SPACES (p, lim)``.  Strictly
speaking, the call expands to a compound statement, which is a complete
statement with no need for a semicolon to end it.  However, since it
looks like a function call, it minimizes confusion if you can use it
like a function call, writing a semicolon afterward, as in
``SKIP_SPACES (p, lim);``

This can cause trouble before ``else`` statements, because the
semicolon is actually a null statement.  Suppose you write

.. code-block:: c++

  if (*p != 0)
    SKIP_SPACES (p, lim);
  else ...

The presence of two statements---the compound statement and a null
statement---in between the ``if`` condition and the ``else``
makes invalid C code.

The definition of the macro ``SKIP_SPACES`` can be altered to solve
this problem, using a ``do ... while`` statement.  Here is how:

.. code-block:: c++

  #define SKIP_SPACES(p, limit)     \
  do { char *lim = (limit);         \
       while (p < lim) {            \
         if (*p++ != ' ') {         \
           p--; break; }}}          \
  while (0)

Now ``SKIP_SPACES (p, lim);`` expands into

.. code-block:: c++

  do {...} while (0);

which is one statement.  The loop executes exactly once; most compilers
generate no extra code for it.

.. index:: side effects (in macro arguments), unsafe macros

.. _duplication-of-side-effects:

Duplication of Side Effects
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Many C programs define a macro ``min``, for 'minimum', like this:

.. code-block:: c++

  #define min(X, Y)  ((X) < (Y) ? (X) : (Y))

When you use this macro with an argument containing a side effect,
as shown here,

.. code-block:: c++

  next = min (x + y, foo (z));

it expands as follows:

.. code-block:: c++

  next = ((x + y) < (foo (z)) ? (x + y) : (foo (z)));

where ``x + y`` has been substituted for ``X`` and ``foo (z)``
for ``Y``.

The function ``foo`` is used only once in the statement as it appears
in the program, but the expression ``foo (z)`` has been substituted
twice into the macro expansion.  As a result, ``foo`` might be called
two times when the statement is executed.  If it has side effects or if
it takes a long time to compute, the results might not be what you
intended.  We say that ``min`` is an :dfn:`unsafe` macro.

The best solution to this problem is to define ``min`` in a way that
computes the value of ``foo (z)`` only once.  The C language offers
no standard way to do this, but it can be done with GNU extensions as
follows:

.. code-block:: c++

  #define min(X, Y)                \
  ({ typeof (X) x_ = (X);          \
     typeof (Y) y_ = (Y);          \
     (x_ < y_) ? x_ : y_; })

The :samp:`({ ... })` notation produces a compound statement that
acts as an expression.  Its value is the value of its last statement.
This permits us to define local variables and assign each argument to
one.  The local variables have underscores after their names to reduce
the risk of conflict with an identifier of wider scope (it is impossible
to avoid this entirely).  Now each argument is evaluated exactly once.

If you do not wish to use GNU C extensions, the only solution is to be
careful when *using* the macro ``min``.  For example, you can
calculate the value of ``foo (z)``, save it in a variable, and use
that variable in ``min`` :

.. code-block:: c++

  #define min(X, Y)  ((X) < (Y) ? (X) : (Y))
  ...
  {
    int tem = foo (z);
    next = min (x + y, tem);
  }

(where we assume that ``foo`` returns type ``int``).

.. index:: self-reference

.. _self-referential-macros:

Self-Referential Macros
^^^^^^^^^^^^^^^^^^^^^^^

A :dfn:`self-referential` macro is one whose name appears in its
definition.  Recall that all macro definitions are rescanned for more
macros to replace.  If the self-reference were considered a use of the
macro, it would produce an infinitely large expansion.  To prevent this,
the self-reference is not considered a macro call.  It is passed into
the preprocessor output unchanged.  Consider an example:

.. code-block:: c++

  #define foo (4 + foo)

where ``foo`` is also a variable in your program.

Following the ordinary rules, each reference to ``foo`` will expand
into ``(4 + foo)`` ; then this will be rescanned and will expand into
``(4 + (4 + foo))`` ; and so on until the computer runs out of memory.

The self-reference rule cuts this process short after one step, at
``(4 + foo)``.  Therefore, this macro definition has the possibly
useful effect of causing the program to add 4 to the value of ``foo``
wherever ``foo`` is referred to.

In most cases, it is a bad idea to take advantage of this feature.  A
person reading the program who sees that ``foo`` is a variable will
not expect that it is a macro as well.  The reader will come across the
identifier ``foo`` in the program and think its value should be that
of the variable ``foo``, whereas in fact the value is four greater.

One common, useful use of self-reference is to create a macro which
expands to itself.  If you write

.. code-block:: c++

  #define EPERM EPERM

then the macro ``EPERM`` expands to ``EPERM``.  Effectively, it is
left alone by the preprocessor whenever it's used in running text.  You
can tell that it's a macro with :samp:`#ifdef`.  You might do this if you
want to define numeric constants with an ``enum``, but have
:samp:`#ifdef` be true for each constant.

If a macro ``x`` expands to use a macro ``y``, and the expansion of
``y`` refers to the macro ``x``, that is an :dfn:`indirect
self-reference` of ``x``.  ``x`` is not expanded in this case
either.  Thus, if we have

.. code-block:: c++

  #define x (4 + y)
  #define y (2 * x)

then ``x`` and ``y`` expand as follows:

.. code-block::

  x    → (4 + y)
       → (4 + (2 * x))

  y    → (2 * x)
       → (2 * (4 + y))

Each macro is expanded when it appears in the definition of the other
macro, but not when it indirectly appears in its own definition.

.. index:: expansion of arguments, macro argument expansion, prescan of macro arguments

.. _argument-prescan:

Argument Prescan
^^^^^^^^^^^^^^^^

Macro arguments are completely macro-expanded before they are
substituted into a macro body, unless they are stringized or pasted
with other tokens.  After substitution, the entire macro body, including
the substituted arguments, is scanned again for macros to be expanded.
The result is that the arguments are scanned *twice* to expand
macro calls in them.

Most of the time, this has no effect.  If the argument contained any
macro calls, they are expanded during the first scan.  The result
therefore contains no macro calls, so the second scan does not change
it.  If the argument were substituted as given, with no prescan, the
single remaining scan would find the same macro calls and produce the
same results.

You might expect the double scan to change the results when a
self-referential macro is used in an argument of another macro
(see :ref:`self-referential-macros`): the self-referential macro would be
expanded once in the first scan, and a second time in the second scan.
However, this is not what happens.  The self-references that do not
expand in the first scan are marked so that they will not expand in the
second scan either.

You might wonder, 'Why mention the prescan, if it makes no difference?
And why not skip it and make the preprocessor faster?'  The answer is
that the prescan does make a difference in three special cases:

* Nested calls to a macro.

  We say that :dfn:`nested` calls to a macro occur when a macro's argument
  contains a call to that very macro.  For example, if ``f`` is a macro
  that expects one argument, ``f (f (1))`` is a nested pair of calls to
  ``f``.  The desired expansion is made by expanding ``f (1)`` and
  substituting that into the definition of ``f``.  The prescan causes
  the expected result to happen.  Without the prescan, ``f (1)`` itself
  would be substituted as an argument, and the inner use of ``f`` would
  appear during the main scan as an indirect self-reference and would not
  be expanded.

* Macros that call other macros that stringize or concatenate.

  If an argument is stringized or concatenated, the prescan does not
  occur.  If you *want* to expand a macro, then stringize or
  concatenate its expansion, you can do that by causing one macro to call
  another macro that does the stringizing or concatenation.  For
  instance, if you have

  .. code-block:: c++

    #define AFTERX(x) X_ ## x
    #define XAFTERX(x) AFTERX(x)
    #define TABLESIZE 1024
    #define BUFSIZE TABLESIZE

  then ``AFTERX(BUFSIZE)`` expands to ``X_BUFSIZE``, and
  ``XAFTERX(BUFSIZE)`` expands to ``X_1024``.  (Not to
  ``X_TABLESIZE``.  Prescan always does a complete expansion.)

* Macros used in arguments, whose expansions contain unshielded commas.

  This can cause a macro expanded on the second scan to be called with the
  wrong number of arguments.  Here is an example:

  .. code-block:: c++

    #define foo  a,b
    #define bar(x) lose(x)
    #define lose(x) (1 + (x))

  We would like ``bar(foo)`` to turn into ``(1 + (foo))``, which
  would then turn into ``(1 + (a,b))``.  Instead, ``bar(foo)``
  expands into ``lose(a,b)``, and you get an error because ``lose``
  requires a single argument.  In this case, the problem is easily solved
  by the same parentheses that ought to be used to prevent misnesting of
  arithmetic operations:

  .. code-block::

    #define foo (a,b)
    or#define bar(x) lose((x))

  The extra pair of parentheses prevents the comma in ``foo`` 's
  definition from being interpreted as an argument separator.

.. index:: newlines in macro arguments

.. _newlines-in-arguments:

Newlines in Arguments
^^^^^^^^^^^^^^^^^^^^^

The invocation of a function-like macro can extend over many logical
lines.  However, in the present implementation, the entire expansion
comes out on one line.  Thus line numbers emitted by the compiler or
debugger refer to the line the invocation started on, which might be
different to the line containing the argument causing the problem.

Here is an example illustrating this:

.. code-block:: c++

  #define ignore_second_arg(a,b,c) a; c

  ignore_second_arg (foo (),
                     ignored (),
                     syntax error);

The syntax error triggered by the tokens ``syntax error`` results in
an error message citing line three---the line of ignore_second_arg---
even though the problematic code comes from line five.

We consider this a bug, and intend to fix it in the near future.