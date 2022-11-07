..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: #ident, #sccs

.. _other-directives:

Other Directives
----------------

The :samp:`#ident` directive takes one argument, a string constant.  On
some systems, that string constant is copied into a special segment of
the object file.  On other systems, the directive is ignored.  The
:samp:`#sccs` directive is a synonym for :samp:`#ident`.

These directives are not part of the C standard, but they are not
official GNU extensions either.  What historical information we have
been able to find, suggests they originated with System V.

.. index:: null directive

The :dfn:`null directive` consists of a :samp:`#` followed by a newline,
with only whitespace (including comments) in between.  A null directive
is understood as a preprocessing directive but has no effect on the
preprocessor output.  The primary significance of the existence of the
null directive is that an input line consisting of just a :samp:`#` will
produce no output, rather than a line of output containing just a
:samp:`#`.  Supposedly some old C programs contain such lines.