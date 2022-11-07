..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Representation of line numbers
******************************

As mentioned above, cpplib stores with each token the line number that
it was lexed on.  In fact, this number is not the number of the line in
the source file, but instead bears more resemblance to the number of the
line in the translation unit.

The preprocessor maintains a monotonic increasing line count, which is
incremented at every new line character (and also at the end of any
buffer that does not end in a new line).  Since a line number of zero is
useful to indicate certain special states and conditions, this variable
starts counting from one.

This variable therefore uniquely enumerates each line in the translation
unit.  With some simple infrastructure, it is straight forward to map
from this to the original source file and line number pair, saving space
whenever line number information needs to be saved.  The code the
implements this mapping lies in the files :samp:`line-map.cc` and
:samp:`line-map.h`.

Command-line macros and assertions are implemented by pushing a buffer
containing the right hand side of an equivalent ``#define`` or
``#assert`` directive.  Some built-in macros are handled similarly.
Since these are all processed before the first line of the main input
file, it will typically have an assigned line closer to twenty than to
one.