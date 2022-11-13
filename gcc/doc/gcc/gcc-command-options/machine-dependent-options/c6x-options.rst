..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: C6X

.. index:: C6X Options

.. _c6x-options:

C6X Options
^^^^^^^^^^^

.. option:: -march={name}

  This specifies the name of the target architecture.  GCC uses this
  name to determine what kind of instructions it can emit when generating
  assembly code.  Permissible names are: :samp:`c62x`,
  :samp:`c64x`, :samp:`c64x+`, :samp:`c67x`, :samp:`c67x+`, :samp:`c674x`.

.. option:: -mbig-endian

  Generate code for a big-endian target.

.. option:: -mlittle-endian

  Generate code for a little-endian target.  This is the default.

.. option:: -msim

  Choose startup files and linker script suitable for the simulator.

.. option:: -msdata=default

  Put small global and static data in the ``.neardata`` section,
  which is pointed to by register ``B14``.  Put small uninitialized
  global and static data in the ``.bss`` section, which is adjacent
  to the ``.neardata`` section.  Put small read-only data into the
  ``.rodata`` section.  The corresponding sections used for large
  pieces of data are ``.fardata``, ``.far`` and ``.const``.

.. option:: -msdata=all

  Put all data, not just small objects, into the sections reserved for
  small data, and use addressing relative to the ``B14`` register to
  access them.

.. option:: -msdata=none

  Make no use of the sections reserved for small data, and use absolute
  addresses to access all data.  Put all initialized global and static
  data in the ``.fardata`` section, and all uninitialized data in the
  ``.far`` section.  Put all constant data into the ``.const``
  section.