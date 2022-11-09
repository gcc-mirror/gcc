..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _ia-64-function-attributes:

IA-64 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported on IA-64 targets:

.. index:: syscall_linkage function attribute, IA-64

.. ia-64-fn-attr:: syscall_linkage

  This attribute is used to modify the IA-64 calling convention by marking
  all input registers as live at all function exits.  This makes it possible
  to restart a system call after an interrupt without having to save/restore
  the input registers.  This also prevents kernel data from leaking into
  application code.

.. index:: version_id function attribute, IA-64

.. ia-64-fn-attr:: version_id

  This IA-64 HP-UX attribute, attached to a global variable or function, renames a
  symbol to contain a version string, thus allowing for function level
  versioning.  HP-UX system header files may use function level versioning
  for some system calls.

  .. code-block:: c++

    extern int foo () __attribute__((version_id ("20040821")));

  Calls to ``foo`` are mapped to calls to ``foo{20040821}``.
