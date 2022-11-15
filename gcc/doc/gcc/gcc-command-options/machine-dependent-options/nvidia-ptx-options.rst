..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: Nvidia PTX

.. index:: Nvidia PTX options, nvptx options

.. _nvidia-ptx-options:

Nvidia PTX Options
^^^^^^^^^^^^^^^^^^

These options are defined for Nvidia PTX:

.. option:: -m64

  Ignored, but preserved for backward compatibility.  Only 64-bit ABI is
  supported.

.. option:: -march={architecture-string}

  Generate code for the specified PTX ISA target architecture
  (e.g. :samp:`sm_35`).  Valid architecture strings are :samp:`sm_30`,
  :samp:`sm_35`, :samp:`sm_53`, :samp:`sm_70`, :samp:`sm_75` and
  :samp:`sm_80`.
  The default depends on how the compiler has been configured, see
  :option:`--with-arch`.

  This option sets the value of the preprocessor macro
  ``__PTX_SM__`` ; for instance, for :samp:`sm_35`, it has the value
  :samp:`350`.

.. option:: -misa={architecture-string}

  Alias of :option:`-march=`.

.. option:: -march-map={architecture-string}

  Select the closest available :option:`-march=` value that is not more
  capable.  For instance, for :option:`-march-map=sm_50` select
  :option:`-march=sm_35`, and for :option:`-march-map=sm_53` select
  :option:`-march=sm_53`.

.. option:: -mptx={version-string}

  Generate code for the specified PTX ISA version (e.g. :samp:`7.0`).
  Valid version strings include :samp:`3.1`, :samp:`6.0`, :samp:`6.3`, and
  :samp:`7.0`.  The default PTX ISA version is 6.0, unless a higher
  version is required for specified PTX ISA target architecture via
  option :option:`-march=`.

  This option sets the values of the preprocessor macros
  ``__PTX_ISA_VERSION_MAJOR__`` and ``__PTX_ISA_VERSION_MINOR__`` ;
  for instance, for :samp:`3.1` the macros have the values :samp:`3` and
  :samp:`1`, respectively.

.. option:: -mmainkernel

  Link in code for a __main kernel.  This is for stand-alone instead of
  offloading execution.

.. option:: -moptimize

  Apply partitioned execution optimizations.  This is the default when any
  level of optimization is selected.

.. option:: -msoft-stack

  Generate code that does not use ``.local`` memory
  directly for stack storage. Instead, a per-warp stack pointer is
  maintained explicitly. This enables variable-length stack allocation (with
  variable-length arrays or ``alloca``), and when global memory is used for
  underlying storage, makes it possible to access automatic variables from other
  threads, or with atomic instructions. This code generation variant is used
  for OpenMP offloading, but the option is exposed on its own for the purpose
  of testing the compiler; to generate code suitable for linking into programs
  using OpenMP offloading, use option :option:`-mgomp`.

.. option:: -muniform-simt

  Switch to code generation variant that allows to execute all threads in each
  warp, while maintaining memory state and side effects as if only one thread
  in each warp was active outside of OpenMP SIMD regions.  All atomic operations
  and calls to runtime (malloc, free, vprintf) are conditionally executed (iff
  current lane index equals the master lane index), and the register being
  assigned is copied via a shuffle instruction from the master lane.  Outside of
  SIMD regions lane 0 is the master; inside, each thread sees itself as the
  master.  Shared memory array ``int __nvptx_uni[]`` stores all-zeros or
  all-ones bitmasks for each warp, indicating current mode (0 outside of SIMD
  regions).  Each thread can bitwise-and the bitmask at position ``tid.y``
  with current lane index to compute the master lane index.

.. option:: -mgomp

  Generate code for use in OpenMP offloading: enables :option:`-msoft-stack` and
  :option:`-muniform-simt` options, and selects corresponding multilib variant.
