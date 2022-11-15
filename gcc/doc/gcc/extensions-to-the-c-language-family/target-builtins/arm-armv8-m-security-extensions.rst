..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _arm-armv8-m-security-extensions:

ARM ARMv8-M Security Extensions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC implements the ARMv8-M Security Extensions as described in the ARMv8-M
Security Extensions: Requirements on Development Tools Engineering
Specification, which can be found at
https://developer.arm.com/documentation/ecm0359818/latest/.

As part of the Security Extensions GCC implements two new function attributes:
``cmse_nonsecure_entry`` and ``cmse_nonsecure_call``.

As part of the Security Extensions GCC implements the intrinsics below.  FPTR
is used here to mean any function pointer type.

.. code-block:: c++

  cmse_address_info_t cmse_TT (void *);
  cmse_address_info_t cmse_TT_fptr (FPTR);
  cmse_address_info_t cmse_TTT (void *);
  cmse_address_info_t cmse_TTT_fptr (FPTR);
  cmse_address_info_t cmse_TTA (void *);
  cmse_address_info_t cmse_TTA_fptr (FPTR);
  cmse_address_info_t cmse_TTAT (void *);
  cmse_address_info_t cmse_TTAT_fptr (FPTR);
  void * cmse_check_address_range (void *, size_t, int);
  typeof(p) cmse_nsfptr_create (FPTR p);
  intptr_t cmse_is_nsfptr (FPTR);
  int cmse_nonsecure_caller (void);
