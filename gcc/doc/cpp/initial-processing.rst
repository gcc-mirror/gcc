..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _initial-processing:

Initial processing
******************

The preprocessor performs a series of textual transformations on its
input.  These happen before all other processing.  Conceptually, they
happen in a rigid order, and the entire file is run through each
transformation before the next one begins.  CPP actually does them
all at once, for performance reasons.  These transformations correspond
roughly to the first three 'phases of translation' described in the C
standard.

.. index:: line endings

* The input file is read into memory and broken into lines.

  Different systems use different conventions to indicate the end of a
  line.  GCC accepts the ASCII control sequences LF, CR
  LF and CR as end-of-line markers.  These are the canonical
  sequences used by Unix, DOS and VMS, and the classic Mac OS (before
  OSX) respectively.  You may therefore safely copy source code written
  on any of those systems to a different one and use it without
  conversion.  (GCC may lose track of the current line number if a file
  doesn't consistently use one convention, as sometimes happens when it
  is edited on computers with different conventions that share a network
  file system.)

  If the last line of any input file lacks an end-of-line marker, the end
  of the file is considered to implicitly supply one.  The C standard says
  that this condition provokes undefined behavior, so GCC will emit a
  warning message.

.. index:: trigraphs

.. _trigraphs:

* If trigraphs are enabled, they are replaced by their
  corresponding single characters.  By default GCC ignores trigraphs,
  but if you request a strictly conforming mode with the :option:`-std`
  option, or you specify the :option:`-trigraphs` option, then it
  converts them.

  These are nine three-character sequences, all starting with :samp:`??`,
  that are defined by ISO C to stand for single characters.  They permit
  obsolete systems that lack some of C's punctuation to use C.  For
  example, :samp:`??/` stands for :samp:`\\`, so ``'??/n'`` is a character
  constant for a newline.

  Trigraphs are not popular and many compilers implement them
  incorrectly.  Portable code should not rely on trigraphs being either
  converted or ignored.  With :option:`-Wtrigraphs` GCC will warn you
  when a trigraph may change the meaning of your program if it were
  converted.  See :ref:`wtrigraphs`.

  In a string constant, you can prevent a sequence of question marks
  from being confused with a trigraph by inserting a backslash between
  the question marks, or by separating the string literal at the
  trigraph and making use of string literal concatenation.  ``"(??\?)"``
  is the string :samp:`(???)`, not :samp:`(?]`.  Traditional C compilers
  do not recognize these idioms.

  The nine trigraphs and their replacements are

  .. code-block::

    Trigraph:       ??(  ??)  ??<  ??>  ??=  ??/  ??'  ??!  ??-
    Replacement:      [    ]    {    }    #    \    ^    |    ~

.. index:: continued lines, backslash-newline

* Continued lines are merged into one long line.

  A continued line is a line which ends with a backslash, :samp:`\\`.  The
  backslash is removed and the following line is joined with the current
  one.  No space is inserted, so you may split a line anywhere, even in
  the middle of a word.  (It is generally more readable to split lines
  only at white space.)

  The trailing backslash on a continued line is commonly referred to as a
  :dfn:`backslash-newline`.

  If there is white space between a backslash and the end of a line, that
  is still a continued line.  However, as this is usually the result of an
  editing mistake, and many compilers will not accept it as a continued
  line, GCC will warn you about it.

.. index:: comments, line comments, block comments

* All comments are replaced with single spaces.

  There are two kinds of comments.  :dfn:`Block comments` begin with
  :samp:`/*` and continue until the next :samp:`*/`.  Block comments do not
  nest:

  .. code-block:: c++

    /* this is /* one comment */ text outside comment

  :dfn:`Line comments` begin with :samp:`//` and continue to the end of the
  current line.  Line comments do not nest either, but it does not matter,
  because they would end in the same place anyway.

  .. code-block:: c++

    // this is // one comment
    text outside comment

It is safe to put line comments inside block comments, or vice versa.

.. code-block:: c++

  /* block comment
     // contains line comment
     yet more comment
   */ outside comment

  // line comment /* contains block comment */

But beware of commenting out one end of a block comment with a line
comment.

.. code-block::

   // l.c.  /* block comment begins
      oops! this isn't a comment anymore */

Comments are not recognized within string literals.
``"/* blah */"`` is the string constant :samp:`/\* blah \*/`, not
an empty string.

Line comments are not in the 1989 edition of the C standard, but they
are recognized by GCC as an extension.  In C++ and in the 1999 edition
of the C standard, they are an official part of the language.

Since these transformations happen before all other processing, you can
split a line mechanically with backslash-newline anywhere.  You can
comment out the end of a line.  You can continue a line comment onto the
next line with backslash-newline.  You can even split :samp:`/*`,
:samp:`*/`, and :samp:`//` onto multiple lines with backslash-newline.
For example:

.. code-block::

  /\
  *
  */ # /*
  */ defi\
  ne FO\
  O 10\
  20

is equivalent to ``#define FOO 1020``.  All these tricks are
extremely confusing and should not be used in code intended to be
readable.

There is no way to prevent a backslash at the end of a line from being
interpreted as a backslash-newline.  This cannot affect any correct
program, however.
