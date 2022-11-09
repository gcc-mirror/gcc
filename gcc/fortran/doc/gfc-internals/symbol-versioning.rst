..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _symbol-versioning:

Symbol Versioning
*****************

In general, this capability exists only on a few platforms, thus there
is a need for configure magic so that it is used only on those targets
where it is supported.

The central concept in symbol versioning is the so-called map file,
which specifies the version node(s) exported symbols are labeled with.
Also, the map file is used to hide local symbols.

Some relevant references:

* `GNU ld manual <https://sourceware.org/binutils/docs/ld/VERSION.html>`_

* `ELF Symbol
  Versioning - Ulrich Depper <https://www.akkadia.org/drepper/symbol-versioning>`_

* `How to Write Shared
  Libraries - Ulrich Drepper (see Chapter 3) <https://www.akkadia.org/drepper/dsohowto.pdf>`_

If one adds a new symbol to a library that should be exported, the new
symbol should be mentioned in the map file and a new version node
defined, e.g., if one adds a new symbols ``foo`` and ``bar`` to
libgfortran for the next GCC release, the following should be added to
the map file:

::

  GFORTRAN_1.1 {
      global:
          foo;
          bar;
  } GFORTRAN_1.0;

where ``GFORTRAN_1.0`` is the version node of the current release,
and ``GFORTRAN_1.1`` is the version node of the next release where
foo and bar are made available.

If one wants to change an existing interface, it is possible by using
some asm trickery (from the :command:`ld` manual referenced above):

.. code-block:: c++

  __asm__(".symver original_foo,foo@");
  __asm__(".symver old_foo,foo@VERS_1.1");
  __asm__(".symver old_foo1,foo@VERS_1.2");
  __asm__(".symver new_foo,foo@VERS_2.0");

In this example, ``foo@`` represents the symbol ``foo`` bound to
the unspecified base version of the symbol. The source file that
contains this example would define 4 C functions: ``original_foo``,
``old_foo``, ``old_foo1``, and ``new_foo``.

In this case the map file must contain ``foo`` in ``VERS_1.1``
and ``VERS_1.2`` as well as in ``VERS_2.0``.
