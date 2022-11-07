..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _powerpc-atomic-memory-operation-functions:

PowerPC Atomic Memory Operation Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

ISA 3.0 of the PowerPC added new atomic memory operation (amo)
instructions.  GCC provides support for these instructions in 64-bit
environments.  All of the functions are declared in the include file
``amo.h``.

The functions supported are:

.. code-block:: c++

  #include <amo.h>

  uint32_t amo_lwat_add (uint32_t *, uint32_t);
  uint32_t amo_lwat_xor (uint32_t *, uint32_t);
  uint32_t amo_lwat_ior (uint32_t *, uint32_t);
  uint32_t amo_lwat_and (uint32_t *, uint32_t);
  uint32_t amo_lwat_umax (uint32_t *, uint32_t);
  uint32_t amo_lwat_umin (uint32_t *, uint32_t);
  uint32_t amo_lwat_swap (uint32_t *, uint32_t);

  int32_t amo_lwat_sadd (int32_t *, int32_t);
  int32_t amo_lwat_smax (int32_t *, int32_t);
  int32_t amo_lwat_smin (int32_t *, int32_t);
  int32_t amo_lwat_sswap (int32_t *, int32_t);

  uint64_t amo_ldat_add (uint64_t *, uint64_t);
  uint64_t amo_ldat_xor (uint64_t *, uint64_t);
  uint64_t amo_ldat_ior (uint64_t *, uint64_t);
  uint64_t amo_ldat_and (uint64_t *, uint64_t);
  uint64_t amo_ldat_umax (uint64_t *, uint64_t);
  uint64_t amo_ldat_umin (uint64_t *, uint64_t);
  uint64_t amo_ldat_swap (uint64_t *, uint64_t);

  int64_t amo_ldat_sadd (int64_t *, int64_t);
  int64_t amo_ldat_smax (int64_t *, int64_t);
  int64_t amo_ldat_smin (int64_t *, int64_t);
  int64_t amo_ldat_sswap (int64_t *, int64_t);

  void amo_stwat_add (uint32_t *, uint32_t);
  void amo_stwat_xor (uint32_t *, uint32_t);
  void amo_stwat_ior (uint32_t *, uint32_t);
  void amo_stwat_and (uint32_t *, uint32_t);
  void amo_stwat_umax (uint32_t *, uint32_t);
  void amo_stwat_umin (uint32_t *, uint32_t);

  void amo_stwat_sadd (int32_t *, int32_t);
  void amo_stwat_smax (int32_t *, int32_t);
  void amo_stwat_smin (int32_t *, int32_t);

  void amo_stdat_add (uint64_t *, uint64_t);
  void amo_stdat_xor (uint64_t *, uint64_t);
  void amo_stdat_ior (uint64_t *, uint64_t);
  void amo_stdat_and (uint64_t *, uint64_t);
  void amo_stdat_umax (uint64_t *, uint64_t);
  void amo_stdat_umin (uint64_t *, uint64_t);

  void amo_stdat_sadd (int64_t *, int64_t);
  void amo_stdat_smax (int64_t *, int64_t);
  void amo_stdat_smin (int64_t *, int64_t);