..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _atomic-builtins:

Built-in Functions for Memory Model Aware Atomic Operations
***********************************************************

The following built-in functions approximately match the requirements
for the C++11 memory model.  They are all
identified by being prefixed with :samp:`__atomic` and most are
overloaded so that they work with multiple types.

These functions are intended to replace the legacy :samp:`__sync`
builtins.  The main difference is that the memory order that is requested
is a parameter to the functions.  New code should always use the
:samp:`__atomic` builtins rather than the :samp:`__sync` builtins.

Note that the :samp:`__atomic` builtins assume that programs will
conform to the C++11 memory model.  In particular, they assume
that programs are free of data races.  See the C++11 standard for
detailed requirements.

The :samp:`__atomic` builtins can be used with any integral scalar or
pointer type that is 1, 2, 4, or 8 bytes in length.  16-byte integral
types are also allowed if :samp:`__int128` (see :ref:`int128`) is
supported by the architecture.

The four non-arithmetic functions (load, store, exchange, and
compare_exchange) all have a generic version as well.  This generic
version works on any data type.  It uses the lock-free built-in function
if the specific data type size makes that possible; otherwise, an
external call is left to be resolved at run time.  This external call is
the same format with the addition of a :samp:`size_t` parameter inserted
as the first parameter indicating the size of the object being pointed to.
All objects must be the same size.

There are 6 different memory orders that can be specified.  These map
to the C++11 memory orders with the same names, see the C++11 standard
or the `GCC wiki
on atomic synchronization <https://gcc.gnu.org/wiki/Atomic/GCCMM/AtomicSync>`_ for detailed definitions.  Individual
targets may also support additional memory orders for use on specific
architectures.  Refer to the target documentation for details of
these.

An atomic operation can both constrain code motion and
be mapped to hardware instructions for synchronization between threads
(e.g., a fence).  To which extent this happens is controlled by the
memory orders, which are listed here in approximately ascending order of
strength.  The description of each memory order is only meant to roughly
illustrate the effects and is not a specification; see the C++11
memory model for precise semantics.

``__ATOMIC_RELAXED``
  Implies no inter-thread ordering constraints.

``__ATOMIC_CONSUME``
  This is currently implemented using the stronger ``__ATOMIC_ACQUIRE``
  memory order because of a deficiency in C++11's semantics for
  ``memory_order_consume``.

``__ATOMIC_ACQUIRE``
  Creates an inter-thread happens-before constraint from the release (or
  stronger) semantic store to this acquire load.  Can prevent hoisting
  of code to before the operation.

``__ATOMIC_RELEASE``
  Creates an inter-thread happens-before constraint to acquire (or stronger)
  semantic loads that read from this release store.  Can prevent sinking
  of code to after the operation.

``__ATOMIC_ACQ_REL``
  Combines the effects of both ``__ATOMIC_ACQUIRE`` and
  ``__ATOMIC_RELEASE``.

``__ATOMIC_SEQ_CST``
  Enforces total ordering with all other ``__ATOMIC_SEQ_CST`` operations.

Note that in the C++11 memory model, *fences* (e.g.,
:samp:`__atomic_thread_fence`) take effect in combination with other
atomic operations on specific memory locations (e.g., atomic loads);
operations on specific memory locations do not necessarily affect other
operations in the same way.

Target architectures are encouraged to provide their own patterns for
each of the atomic built-in functions.  If no target is provided, the original
non-memory model set of :samp:`__sync` atomic built-in functions are
used, along with any required synchronization fences surrounding it in
order to achieve the proper behavior.  Execution in this case is subject
to the same restrictions as those built-in functions.

If there is no pattern or mechanism to provide a lock-free instruction
sequence, a call is made to an external routine with the same parameters
to be resolved at run time.

When implementing patterns for these built-in functions, the memory order
parameter can be ignored as long as the pattern implements the most
restrictive ``__ATOMIC_SEQ_CST`` memory order.  Any of the other memory
orders execute correctly with this memory order but they may not execute as
efficiently as they could with a more appropriate implementation of the
relaxed requirements.

Note that the C++11 standard allows for the memory order parameter to be
determined at run time rather than at compile time.  These built-in
functions map any run-time value to ``__ATOMIC_SEQ_CST`` rather
than invoke a runtime library call or inline a switch statement.  This is
standard compliant, safe, and the simplest approach for now.

The memory order parameter is a signed int, but only the lower 16 bits are
reserved for the memory order.  The remainder of the signed int is reserved
for target use and should be 0.  Use of the predefined atomic values
ensures proper usage.

.. function:: type __atomic_load_n (type *ptr, int memorder)

  This built-in function implements an atomic load operation.  It returns the
  contents of ``*ptr``.

  The valid memory order variants are
  ``__ATOMIC_RELAXED``, ``__ATOMIC_SEQ_CST``, ``__ATOMIC_ACQUIRE``,
  and ``__ATOMIC_CONSUME``.

.. function:: void __atomic_load (type *ptr, type *ret, int memorder)

  This is the generic version of an atomic load.  It returns the
  contents of ``*ptr`` in ``*ret``.

.. function:: void __atomic_store_n (type *ptr, type val, int memorder)

  This built-in function implements an atomic store operation.  It writes
  ``val`` into ``*ptr``.

  The valid memory order variants are
  ``__ATOMIC_RELAXED``, ``__ATOMIC_SEQ_CST``, and ``__ATOMIC_RELEASE``.

.. function:: void __atomic_store (type *ptr, type *val, int memorder)

  This is the generic version of an atomic store.  It stores the value
  of ``*val`` into ``*ptr``.

.. function:: type __atomic_exchange_n (type *ptr, type val, int memorder)

  This built-in function implements an atomic exchange operation.  It writes
  :samp:`{val}` into ``*ptr``, and returns the previous contents of
  ``*ptr``.

  All memory order variants are valid.

.. function:: void __atomic_exchange (type *ptr, type *val, type *ret, int memorder)

  This is the generic version of an atomic exchange.  It stores the
  contents of ``*val`` into ``*ptr``. The original value
  of ``*ptr`` is copied into ``*ret``.

.. function:: bool __atomic_compare_exchange_n (type *ptr, type *expected, type desired, bool weak, int success_memorder, int failure_memorder)

  This built-in function implements an atomic compare and exchange operation.
  This compares the contents of ``*ptr`` with the contents of
  ``*expected``. If equal, the operation is a *read-modify-write*
  operation that writes :samp:`{desired}` into ``*ptr``.  If they are not
  equal, the operation is a *read* and the current contents of
  ``*ptr`` are written into ``*expected``.  :samp:`{weak}` is ``true``
  for weak compare_exchange, which may fail spuriously, and ``false`` for
  the strong variation, which never fails spuriously.  Many targets
  only offer the strong variation and ignore the parameter.  When in doubt, use
  the strong variation.

  If :samp:`{desired}` is written into ``*ptr`` then ``true`` is returned
  and memory is affected according to the
  memory order specified by :samp:`{success_memorder}`.  There are no
  restrictions on what memory order can be used here.

  Otherwise, ``false`` is returned and memory is affected according
  to :samp:`{failure_memorder}`. This memory order cannot be
  ``__ATOMIC_RELEASE`` nor ``__ATOMIC_ACQ_REL``.  It also cannot be a
  stronger order than that specified by :samp:`{success_memorder}`.

.. function:: bool __atomic_compare_exchange (type *ptr, type *expected, type *desired, bool weak, int success_memorder, int failure_memorder)

  This built-in function implements the generic version of
  ``__atomic_compare_exchange``.  The function is virtually identical to
  ``__atomic_compare_exchange_n``, except the desired value is also a
  pointer.

.. function:: type __atomic_add_fetch (type *ptr, type val, int memorder)
              type __atomic_sub_fetch (type *ptr, type val, int memorder)
              type __atomic_and_fetch (type *ptr, type val, int memorder)
              type __atomic_xor_fetch (type *ptr, type val, int memorder)
              type __atomic_or_fetch (type *ptr, type val, int memorder)
              type __atomic_nand_fetch (type *ptr, type val, int memorder)

  These built-in functions perform the operation suggested by the name, and
  return the result of the operation.  Operations on pointer arguments are
  performed as if the operands were of the ``uintptr_t`` type.  That is,
  they are not scaled by the size of the type to which the pointer points.

  .. code-block:: c++

    { *ptr op= val; return *ptr; }
    { *ptr = ~(*ptr & val); return *ptr; } // nand

  The object pointed to by the first argument must be of integer or pointer
  type.  It must not be a boolean type.  All memory orders are valid.

.. function:: type __atomic_fetch_add (type *ptr, type val, int memorder)
              type __atomic_fetch_sub (type *ptr, type val, int memorder)
              type __atomic_fetch_and (type *ptr, type val, int memorder)
              type __atomic_fetch_xor (type *ptr, type val, int memorder)
              type __atomic_fetch_or (type *ptr, type val, int memorder)
              type __atomic_fetch_nand (type *ptr, type val, int memorder)

  These built-in functions perform the operation suggested by the name, and
  return the value that had previously been in ``*ptr``.  Operations
  on pointer arguments are performed as if the operands were of
  the ``uintptr_t`` type.  That is, they are not scaled by the size of
  the type to which the pointer points.

  .. code-block:: c++

    { tmp = *ptr; *ptr op= val; return tmp; }
    { tmp = *ptr; *ptr = ~(*ptr & val); return tmp; } // nand

  The same constraints on arguments apply as for the corresponding
  ``__atomic_op_fetch`` built-in functions.  All memory orders are valid.

.. function:: bool __atomic_test_and_set (void *ptr, int memorder)

  This built-in function performs an atomic test-and-set operation on
  the byte at ``*ptr``.  The byte is set to some implementation
  defined nonzero 'set' value and the return value is ``true`` if and only
  if the previous contents were 'set'.
  It should be only used for operands of type ``bool`` or ``char``. For
  other types only part of the value may be set.

  All memory orders are valid.

.. function:: void __atomic_clear (bool *ptr, int memorder)

  This built-in function performs an atomic clear operation on
  ``*ptr``.  After the operation, ``*ptr`` contains 0.
  It should be only used for operands of type ``bool`` or ``char`` and
  in conjunction with ``__atomic_test_and_set``.
  For other types it may only clear partially. If the type is not ``bool``
  prefer using ``__atomic_store``.

  The valid memory order variants are
  ``__ATOMIC_RELAXED``, ``__ATOMIC_SEQ_CST``, and
  ``__ATOMIC_RELEASE``.

.. function:: void __atomic_thread_fence (int memorder)

  This built-in function acts as a synchronization fence between threads
  based on the specified memory order.

  All memory orders are valid.

.. function:: void __atomic_signal_fence (int memorder)

  This built-in function acts as a synchronization fence between a thread
  and signal handlers based in the same thread.

  All memory orders are valid.

.. function:: bool __atomic_always_lock_free (size_t size,  void *ptr)

  This built-in function returns ``true`` if objects of :samp:`{size}` bytes always
  generate lock-free atomic instructions for the target architecture.
  :samp:`{size}` must resolve to a compile-time constant and the result also
  resolves to a compile-time constant.

  :samp:`{ptr}` is an optional pointer to the object that may be used to determine
  alignment.  A value of 0 indicates typical alignment should be used.  The
  compiler may also ignore this parameter.

  .. code-block:: c++

    if (__atomic_always_lock_free (sizeof (long long), 0))

.. function:: bool __atomic_is_lock_free (size_t size, void *ptr)

  This built-in function returns ``true`` if objects of :samp:`{size}` bytes always
  generate lock-free atomic instructions for the target architecture.  If
  the built-in function is not known to be lock-free, a call is made to a
  runtime routine named ``__atomic_is_lock_free``.

  :samp:`{ptr}` is an optional pointer to the object that may be used to determine
  alignment.  A value of 0 indicates typical alignment should be used.  The
  compiler may also ignore this parameter.