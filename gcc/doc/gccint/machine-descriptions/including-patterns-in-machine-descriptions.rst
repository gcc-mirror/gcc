..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: insn includes, include

.. _including-patterns:

Including Patterns in Machine Descriptions.
*******************************************

The ``include`` pattern tells the compiler tools where to
look for patterns that are in files other than in the file
:samp:`.md`.  This is used only at build time and there is no preprocessing allowed.

It looks like:

.. code-block:: c++

  (include
    pathname)

For example:

.. code-block:: c++

  (include "filestuff")

Where :samp:`{pathname}` is a string that specifies the location of the file,
specifies the include file to be in :samp:`gcc/config/target/filestuff`.  The
directory :samp:`gcc/config/target` is regarded as the default directory.

Machine descriptions may be split up into smaller more manageable subsections
and placed into subdirectories.

By specifying:

.. code-block:: c++

  (include "BOGUS/filestuff")

the include file is specified to be in :samp:`gcc/config/{target}/BOGUS/filestuff`.

Specifying an absolute path for the include file such as;

.. code-block:: c++

  (include "/u2/BOGUS/filestuff")

is permitted but is not encouraged.

.. index:: directory options .md, options, directory search, search options

RTL Generation Tool Options for Directory Search
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :option:`-Idir` option specifies directories to search for machine descriptions.
For example:

.. code-block:: c++

  genrecog -I/p1/abc/proc1 -I/p2/abcd/pro2 target.md

Add the directory :samp:`{dir}` to the head of the list of directories to be
searched for header files.  This can be used to override a system machine definition
file, substituting your own version, since these directories are
searched before the default machine description file directories.  If you use more than
one :option:`-I` option, the directories are scanned in left-to-right
order; the standard default directory come after.
