..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _x86-transactional-memory-intrinsics:

x86 Transactional Memory Intrinsics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These hardware transactional memory intrinsics for x86 allow you to use
memory transactions with RTM (Restricted Transactional Memory).
This support is enabled with the :option:`-mrtm` option.
For using HLE (Hardware Lock Elision) see
:ref:`x86-specific-memory-model-extensions-for-transactional-memory` instead.

A memory transaction commits all changes to memory in an atomic way,
as visible to other threads. If the transaction fails it is rolled back
and all side effects discarded.

Generally there is no guarantee that a memory transaction ever succeeds
and suitable fallback code always needs to be supplied.

.. function:: unsigned _xbegin ()

  Start a RTM (Restricted Transactional Memory) transaction.
  Returns ``_XBEGIN_STARTED`` when the transaction
  started successfully (note this is not 0, so the constant has to be
  explicitly tested).

  If the transaction aborts, all side effects
  are undone and an abort code encoded as a bit mask is returned.
  The following macros are defined:

  ``_XABORT_EXPLICIT``
    Transaction was explicitly aborted with ``_xabort``.  The parameter passed
    to ``_xabort`` is available with ``_XABORT_CODE(status)``.

  ``_XABORT_RETRY``
    Transaction retry is possible.

  ``_XABORT_CONFLICT``
    Transaction abort due to a memory conflict with another thread.

  ``_XABORT_CAPACITY``
    Transaction abort due to the transaction using too much memory.

  ``_XABORT_DEBUG``
    Transaction abort due to a debug trap.

  ``_XABORT_NESTED``
    Transaction abort in an inner nested transaction.

  There is no guarantee
  any transaction ever succeeds, so there always needs to be a valid
  fallback path.

.. function:: void _xend ()

  Commit the current transaction. When no transaction is active this faults.
  All memory side effects of the transaction become visible
  to other threads in an atomic manner.

.. function:: int _xtest ()

  Return a nonzero value if a transaction is currently active, otherwise 0.

.. function:: void _xabort (status)

  Abort the current transaction. When no transaction is active this is a no-op.
  The :samp:`{status}` is an 8-bit constant; its value is encoded in the return
  value from ``_xbegin``.

Here is an example showing handling for ``_XABORT_RETRY``
and a fallback path for other failures:

.. code-block:: c++

  #include <immintrin.h>

  int n_tries, max_tries;
  unsigned status = _XABORT_EXPLICIT;
  ...

  for (n_tries = 0; n_tries < max_tries; n_tries++)
    {
      status = _xbegin ();
      if (status == _XBEGIN_STARTED || !(status & _XABORT_RETRY))
        break;
    }
  if (status == _XBEGIN_STARTED)
    {
      ... transaction code...
      _xend ();
    }
  else
    {
      ... non-transactional fallback path...
    }

Note that, in most cases, the transactional and non-transactional code
must synchronize together to ensure consistency.
