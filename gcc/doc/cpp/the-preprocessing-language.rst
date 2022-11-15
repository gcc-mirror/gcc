..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: directives, preprocessing directives, directive line, directive name

.. _the-preprocessing-language:

The preprocessing language
**************************

After tokenization, the stream of tokens may simply be passed straight
to the compiler's parser.  However, if it contains any operations in the
:dfn:`preprocessing language`, it will be transformed first.  This stage
corresponds roughly to the standard's 'translation phase 4' and is
what most people think of as the preprocessor's job.

The preprocessing language consists of :dfn:`directives` to be executed
and :dfn:`macros` to be expanded.  Its primary capabilities are:

* Inclusion of header files.  These are files of declarations that can be
  substituted into your program.

* Macro expansion.  You can define :dfn:`macros`, which are abbreviations
  for arbitrary fragments of C code.  The preprocessor will replace the
  macros with their definitions throughout the program.  Some macros are
  automatically defined for you.

* Conditional compilation.  You can include or exclude parts of the
  program according to various conditions.

* Line control.  If you use a program to combine or rearrange source files
  into an intermediate file which is then compiled, you can use line
  control to inform the compiler where each source line originally came
  from.

* Diagnostics.  You can detect problems at compile time and issue errors
  or warnings.

There are a few more, less useful, features.

Except for expansion of predefined macros, all these operations are
triggered with :dfn:`preprocessing directives`.  Preprocessing directives
are lines in your program that start with :samp:`#`.  Whitespace is
allowed before and after the :samp:`#`.  The :samp:`#` is followed by an
identifier, the :dfn:`directive name`.  It specifies the operation to
perform.  Directives are commonly referred to as :samp:`#{name}`
where :samp:`{name}` is the directive name.  For example, :samp:`#define` is
the directive that defines a macro.

The :samp:`#` which begins a directive cannot come from a macro
expansion.  Also, the directive name is not macro expanded.  Thus, if
``foo`` is defined as a macro expanding to ``define``, that does
not make :samp:`#foo` a valid preprocessing directive.

The set of valid directive names is fixed.  Programs cannot define new
preprocessing directives.

Some directives require arguments; these make up the rest of the
directive line and must be separated from the directive name by
whitespace.  For example, :samp:`#define` must be followed by a macro
name and the intended expansion of the macro.

A preprocessing directive cannot cover more than one line.  The line
may, however, be continued with backslash-newline, or by a block comment
which extends past the end of the line.  In either case, when the
directive is processed, the continuations have already been merged with
the first line to make one long line.
