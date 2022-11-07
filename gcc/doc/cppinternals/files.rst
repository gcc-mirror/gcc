.. _files:

File Handling
=============

.. index:: files

Fairly obviously, the file handling code of cpplib resides in the file
:samp:`files.c`.  It takes care of the details of file searching,
opening, reading and caching, for both the main source file and all the
headers it recursively includes.

The basic strategy is to minimize the number of system calls.  On many
systems, the basic ``open ()`` and ``fstat ()`` system calls can
be quite expensive.  For every ``#include`` -d file, we need to try
all the directories in the search path until we find a match.  Some
projects, such as glibc, pass twenty or thirty include paths on the
command line, so this can rapidly become time consuming.

For a header file we have not encountered before we have little choice
but to do this.  However, it is often the case that the same headers are
repeatedly included, and in these cases we try to avoid repeating the
filesystem queries whilst searching for the correct file.

For each file we try to open, we store the constructed path in a splay
tree.  This path first undergoes simplification by the function
``_cpp_simplify_pathname``.  For example,
:samp:`/usr/include/bits/../foo.h` is simplified to
:samp:`/usr/include/foo.h` before we enter it in the splay tree and try
to ``open ()`` the file.  CPP will then find subsequent uses of
:samp:`foo.h`, even as :samp:`/usr/include/foo.h`, in the splay tree and
save system calls.

Further, it is likely the file contents have also been cached, saving a
``read ()`` system call.  We don't bother caching the contents of
header files that are re-inclusion protected, and whose re-inclusion
macro is defined when we leave the header file for the first time.  If
the host supports it, we try to map suitably large files into memory,
rather than reading them in directly.

The include paths are internally stored on a null-terminated
singly-linked list, starting with the ``"header.h"`` directory search
chain, which then links into the ``<header.h>`` directory chain.

Files included with the ``<foo.h>`` syntax start the lookup directly
in the second half of this chain.  However, files included with the
``"foo.h"`` syntax start at the beginning of the chain, but with one
extra directory prepended.  This is the directory of the current file;
the one containing the ``#include`` directive.  Prepending this
directory on a per-file basis is handled by the function
``search_from``.

Note that a header included with a directory component, such as
``#include "mydir/foo.h"`` and opened as
:samp:`/usr/local/include/mydir/foo.h`, will have the complete path minus
the basename :samp:`foo.h` as the current directory.

Enough information is stored in the splay tree that CPP can immediately
tell whether it can skip the header file because of the multiple include
optimization, whether the file didn't exist or couldn't be opened for
some reason, or whether the header was flagged not to be re-used, as it
is with the obsolete ``#import`` directive.

For the benefit of MS-DOS filesystems with an 8.3 filename limitation,
CPP offers the ability to treat various include file names as aliases
for the real header files with shorter names.  The map from one to the
other is found in a special file called :samp:`header.gcc`, stored in the
command line (or system) include directories to which the mapping
applies.  This may be higher up the directory tree than the full path to
the file minus the base name.