..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _collect2:

collect2
--------

GCC uses a utility called ``collect2`` on nearly all systems to arrange
to call various initialization functions at start time.

The program ``collect2`` works by linking the program once and
looking through the linker output file for symbols with particular names
indicating they are constructor functions.  If it finds any, it
creates a new temporary :samp:`.c` file containing a table of them,
compiles it, and links the program a second time including that file.

.. index:: __main, constructors, automatic calls

The actual calls to the constructors are carried out by a subroutine
called ``__main``, which is called (automatically) at the beginning
of the body of ``main`` (provided ``main`` was compiled with GNU
CC).  Calling ``__main`` is necessary, even when compiling C code, to
allow linking C and C++ object code together.  (If you use
:option:`-nostdlib`, you get an unresolved reference to ``__main``,
since it's defined in the standard GCC library.  Include :option:`-lgcc` at
the end of your compiler command line to resolve this reference.)

The program ``collect2`` is installed as ``ld`` in the directory
where the passes of the compiler are installed.  When ``collect2``
needs to find the *real* ``ld``, it tries the following file
names:

* a hard coded linker file name, if GCC was configured with the
  :option:`--with-ld` option.

* :samp:`real-ld` in the directories listed in the compiler's search
  directories.

* :samp:`real-ld` in the directories listed in the environment variable
  ``PATH``.

* The file specified in the ``REAL_LD_FILE_NAME`` configuration macro,
  if specified.

* :samp:`ld` in the compiler's search directories, except that
  ``collect2`` will not execute itself recursively.

* :samp:`ld` in ``PATH``.

'The compiler's search directories' means all the directories where
:command:`gcc` searches for passes of the compiler.  This includes
directories that you specify with :option:`-B`.

Cross-compilers search a little differently:

* :samp:`real-ld` in the compiler's search directories.

* :samp:`{target}-real-ld` in ``PATH``.

* The file specified in the ``REAL_LD_FILE_NAME`` configuration macro,
  if specified.

* :samp:`ld` in the compiler's search directories.

* :samp:`{target}-ld` in ``PATH``.

``collect2`` explicitly avoids running ``ld`` using the file name
under which ``collect2`` itself was invoked.  In fact, it remembers
up a list of such names---in case one copy of ``collect2`` finds
another copy (or version) of ``collect2`` installed as ``ld`` in a
second place in the search path.

``collect2`` searches for the utilities ``nm`` and ``strip``
using the same algorithm as above for ``ld``.