..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _nvidia-ptx-function-attributes:

Nvidia PTX Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Nvidia PTX back end:

.. index:: kernel attribute, Nvidia PTX

.. nvidia-ptx-fn-attr:: kernel

  This attribute indicates that the corresponding function should be compiled
  as a kernel function, which can be invoked from the host via the CUDA RT
  library.
  By default functions are only callable only from other PTX functions.

  Kernel functions must have ``void`` return type.
