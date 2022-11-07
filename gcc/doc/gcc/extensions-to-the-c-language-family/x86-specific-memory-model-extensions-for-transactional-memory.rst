..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _x86-specific-memory-model-extensions-for-transactional-memory:

x86-Specific Memory Model Extensions for Transactional Memory
*************************************************************

The x86 architecture supports additional memory ordering flags
to mark critical sections for hardware lock elision.
These must be specified in addition to an existing memory order to
atomic intrinsics.

``__ATOMIC_HLE_ACQUIRE``
  Start lock elision on a lock variable.
  Memory order must be ``__ATOMIC_ACQUIRE`` or stronger.

``__ATOMIC_HLE_RELEASE``
  End lock elision on a lock variable.
  Memory order must be ``__ATOMIC_RELEASE`` or stronger.

When a lock acquire fails, it is required for good performance to abort
the transaction quickly. This can be done with a ``_mm_pause``.

.. code-block:: c++

  #include <immintrin.h> // For _mm_pause

  int lockvar;

  /* Acquire lock with lock elision */
  while (__atomic_exchange_n(&lockvar, 1, __ATOMIC_ACQUIRE|__ATOMIC_HLE_ACQUIRE))
      _mm_pause(); /* Abort failed transaction */
  ...
  /* Free lock with lock elision */
  __atomic_store_n(&lockvar, 0, __ATOMIC_RELEASE|__ATOMIC_HLE_RELEASE);