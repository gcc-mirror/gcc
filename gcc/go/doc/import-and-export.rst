..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _import-and-export:

Import and Export
-----------------

When :command:`gccgo` compiles a package which exports anything, the
export information will be stored directly in the object file.  When a
package is imported, :command:`gccgo` must be able to find the file.

.. index:: .gox

When Go code imports the package :samp:`{gopackage}`, :command:`gccgo`
will look for the import data using the following filenames, using the
first one that it finds.

.. code-block::

  gopackage.gox
  libgopackage.so
  libgopackage.a
  gopackage.o

The compiler will search for these files in the directories named by
any :option:`-I` options, in order in which the directories appear on
the command line.  The compiler will then search several standard
system directories.  Finally the compiler will search the current
directory (to search the current directory earlier, use :samp:`-I.`).

The compiler will extract the export information directly from the
compiled object file.  The file :samp:`{gopackage}.gox` will
typically contain nothing but export data.  This can be generated from
:samp:`{gopackage}.o` via

.. code-block:: c++

  objcopy -j .go_export gopackage.o gopackage.gox

For example, it may be desirable to extract the export information
from several different packages into their independent
:samp:`{gopackage}.gox` files, and then to combine the different
package object files together into a single shared library or archive.

At link time you must explicitly tell :command:`gccgo` which files to
link together into the executable, as is usual with :command:`gcc`.
This is different from the behavior of other Go compilers.