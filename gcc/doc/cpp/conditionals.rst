..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: conditionals

.. _conditionals:

Conditionals
------------

A :dfn:`conditional` is a directive that instructs the preprocessor to
select whether or not to include a chunk of code in the final token
stream passed to the compiler.  Preprocessor conditionals can test
arithmetic expressions, or whether a name is defined as a macro, or both
simultaneously using the special ``defined`` operator.

A conditional in the C preprocessor resembles in some ways an ``if``
statement in C, but it is important to understand the difference between
them.  The condition in an ``if`` statement is tested during the
execution of your program.  Its purpose is to allow your program to
behave differently from run to run, depending on the data it is
operating on.  The condition in a preprocessing conditional directive is
tested when your program is compiled.  Its purpose is to allow different
code to be included in the program depending on the situation at the
time of compilation.

However, the distinction is becoming less clear.  Modern compilers often
do test ``if`` statements when a program is compiled, if their
conditions are known not to vary at run time, and eliminate code which
can never be executed.  If you can count on your compiler to do this,
you may find that your program is more readable if you use ``if``
statements with constant conditions (perhaps determined by macros).  Of
course, you can only use this to exclude code, not type definitions or
other preprocessing directives, and you can only do it if the code
remains syntactically valid when it is not to be used.

.. toctree::
  :maxdepth: 2

  conditional-uses
  conditional-syntax
  deleted-code
