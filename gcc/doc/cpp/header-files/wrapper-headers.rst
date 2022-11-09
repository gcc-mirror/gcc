..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: wrapper headers, overriding a header file, #include_next

.. _wrapper-headers:

Wrapper Headers
***************

Sometimes it is necessary to adjust the contents of a system-provided
header file without editing it directly.  GCC's :command:`fixincludes`
operation does this, for example.  One way to do that would be to create
a new header file with the same name and insert it in the search path
before the original header.  That works fine as long as you're willing
to replace the old header entirely.  But what if you want to refer to
the old header from the new one?

You cannot simply include the old header with :samp:`#include`.  That
will start from the beginning, and find your new header again.  If your
header is not protected from multiple inclusion (see :ref:`once-only-headers`), it will recurse infinitely and cause a fatal error.

You could include the old header with an absolute pathname:

.. code-block:: c++

  #include "/usr/include/old-header.h"

This works, but is not clean; should the system headers ever move, you
would have to edit the new headers to match.

There is no way to solve this problem within the C standard, but you can
use the GNU extension :samp:`#include_next`.  It means, 'Include the
*next* file with this name'.  This directive works like
:samp:`#include` except in searching for the specified file: it starts
searching the list of header file directories *after* the directory
in which the current file was found.

Suppose you specify :option:`-I /usr/local/include`, and the list of
directories to search also includes :samp:`/usr/include`; and suppose
both directories contain :samp:`signal.h`.  Ordinary ``#include
<signal.h>`` finds the file under :samp:`/usr/local/include`.  If that
file contains ``#include_next <signal.h>``, it starts searching
after that directory, and finds the file in :samp:`/usr/include`.

:samp:`#include_next` does not distinguish between ``<file>``
and ``"file"`` inclusion, nor does it check that the file you
specify has the same name as the current file.  It simply looks for the
file named, starting with the directory in the search path after the one
where the current file was found.

The use of :samp:`#include_next` can lead to great confusion.  We
recommend it be used only when there is no other alternative.  In
particular, it should not be used in the headers belonging to a specific
program; it should be used only to make global corrections along the
lines of :command:`fixincludes`.
