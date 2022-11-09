..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: line control

.. _line-control:

Line Control
------------

The C preprocessor informs the C compiler of the location in your source
code where each token came from.  Presently, this is just the file name
and line number.  All the tokens resulting from macro expansion are
reported as having appeared on the line of the source file where the
outermost macro was used.  We intend to be more accurate in the future.

If you write a program which generates source code, such as the
:command:`bison` parser generator, you may want to adjust the preprocessor's
notion of the current file name and line number by hand.  Parts of the
output from :command:`bison` are generated from scratch, other parts come
from a standard parser file.  The rest are copied verbatim from
:command:`bison`'s input.  You would like compiler error messages and
symbolic debuggers to be able to refer to ``bison`` 's input file.

.. index:: #line

:command:`bison` or any such program can arrange this by writing
:samp:`#line` directives into the output file.  :samp:`#line` is a
directive that specifies the original line number and source file name
for subsequent input in the current preprocessor input file.
:samp:`#line` has three variants:

:samp:`#line {linenum}`
  :samp:`{linenum}` is a non-negative decimal integer constant.  It specifies
  the line number which should be reported for the following line of
  input.  Subsequent lines are counted from :samp:`{linenum}`.

:samp:`#line {linenum}{filename}`
  :samp:`{linenum}` is the same as for the first form, and has the same
  effect.  In addition, :samp:`{filename}` is a string constant.  The
  following line and all subsequent lines are reported to come from the
  file it specifies, until something else happens to change that.
  :samp:`{filename}` is interpreted according to the normal rules for a string
  constant: backslash escapes are interpreted.  This is different from
  :samp:`#include`.

:samp:`#line {anything else}`
  :samp:`{anything else}` is checked for macro calls, which are expanded.
  The result should match one of the above two forms.

:samp:`#line` directives alter the results of the ``__FILE__`` and
``__LINE__`` predefined macros from that point on.  See :ref:`standard-predefined-macros`.  They do not have any effect on :samp:`#include`'s
idea of the directory containing the current file.
