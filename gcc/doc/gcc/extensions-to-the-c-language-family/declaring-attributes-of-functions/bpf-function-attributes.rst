..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _bpf-function-attributes:

BPF Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the BPF back end:

.. index:: kernel helper, function attribute, BPF

.. bpf-fn-attr:: kernel_helper

  use this attribute to indicate the specified function declaration is a
  kernel helper.  The helper function is passed as an argument to the
  attribute.  Example:

  .. code-block:: c++

    int bpf_probe_read (void *dst, int size, const void *unsafe_ptr)
      __attribute__ ((kernel_helper (4)));
