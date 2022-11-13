..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _powerpc-hardware-transactional-memory-built-in-functions:

PowerPC Hardware Transactional Memory Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides two interfaces for accessing the Hardware Transactional
Memory (HTM) instructions available on some of the PowerPC family
of processors (eg, POWER8).  The two interfaces come in a low level
interface, consisting of built-in functions specific to PowerPC and a
higher level interface consisting of inline functions that are common
between PowerPC and S/390.

PowerPC HTM Low Level Built-in Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following low level built-in functions are available with
:option:`-mhtm` or :option:`-mcpu=CPU` where CPU is 'power8' or later.
They all generate the machine instruction that is part of the name.

The HTM builtins (with the exception of ``__builtin_tbegin``) return
the full 4-bit condition register value set by their associated hardware
instruction.  The header file ``htmintrin.h`` defines some macros that can
be used to decipher the return value.  The ``__builtin_tbegin`` builtin
returns a simple ``true`` or ``false`` value depending on whether a transaction was
successfully started or not.  The arguments of the builtins match exactly the
type and order of the associated hardware instruction's operands, except for
the ``__builtin_tcheck`` builtin, which does not take any input arguments.
Refer to the ISA manual for a description of each instruction's operands.

.. code-block:: c++

  unsigned int __builtin_tbegin (unsigned int);
  unsigned int __builtin_tend (unsigned int);

  unsigned int __builtin_tabort (unsigned int);
  unsigned int __builtin_tabortdc (unsigned int, unsigned int, unsigned int);
  unsigned int __builtin_tabortdci (unsigned int, unsigned int, int);
  unsigned int __builtin_tabortwc (unsigned int, unsigned int, unsigned int);
  unsigned int __builtin_tabortwci (unsigned int, unsigned int, int);

  unsigned int __builtin_tcheck (void);
  unsigned int __builtin_treclaim (unsigned int);
  unsigned int __builtin_trechkpt (void);
  unsigned int __builtin_tsr (unsigned int);

In addition to the above HTM built-ins, we have added built-ins for
some common extended mnemonics of the HTM instructions:

.. code-block:: c++

  unsigned int __builtin_tendall (void);
  unsigned int __builtin_tresume (void);
  unsigned int __builtin_tsuspend (void);

Note that the semantics of the above HTM builtins are required to mimic
the locking semantics used for critical sections.  Builtins that are used
to create a new transaction or restart a suspended transaction must have
lock acquisition like semantics while those builtins that end or suspend a
transaction must have lock release like semantics.  Specifically, this must
mimic lock semantics as specified by C++11, for example: Lock acquisition is
as-if an execution of __atomic_exchange_n(&globallock,1,__ATOMIC_ACQUIRE)
that returns 0, and lock release is as-if an execution of
__atomic_store(&globallock,0,__ATOMIC_RELEASE), with globallock being an
implicit implementation-defined lock used for all transactions.  The HTM
instructions associated with with the builtins inherently provide the
correct acquisition and release hardware barriers required.  However,
the compiler must also be prohibited from moving loads and stores across
the builtins in a way that would violate their semantics.  This has been
accomplished by adding memory barriers to the associated HTM instructions
(which is a conservative approach to provide acquire and release semantics).
Earlier versions of the compiler did not treat the HTM instructions as
memory barriers.  A ``__TM_FENCE__`` macro has been added, which can
be used to determine whether the current compiler treats HTM instructions
as memory barriers or not.  This allows the user to explicitly add memory
barriers to their code when using an older version of the compiler.

The following set of built-in functions are available to gain access
to the HTM specific special purpose registers.

.. code-block:: c++

  unsigned long __builtin_get_texasr (void);
  unsigned long __builtin_get_texasru (void);
  unsigned long __builtin_get_tfhar (void);
  unsigned long __builtin_get_tfiar (void);

  void __builtin_set_texasr (unsigned long);
  void __builtin_set_texasru (unsigned long);
  void __builtin_set_tfhar (unsigned long);
  void __builtin_set_tfiar (unsigned long);

Example usage of these low level built-in functions may look like:

.. code-block:: c++

  #include <htmintrin.h>

  int num_retries = 10;

  while (1)
    {
      if (__builtin_tbegin (0))
        {
          /* Transaction State Initiated.  */
          if (is_locked (lock))
            __builtin_tabort (0);
          ... transaction code...
          __builtin_tend (0);
          break;
        }
      else
        {
          /* Transaction State Failed.  Use locks if the transaction
             failure is "persistent" or we've tried too many times.  */
          if (num_retries-- <= 0
              || _TEXASRU_FAILURE_PERSISTENT (__builtin_get_texasru ()))
            {
              acquire_lock (lock);
              ... non transactional fallback path...
              release_lock (lock);
              break;
            }
        }
    }

One final built-in function has been added that returns the value of
the 2-bit Transaction State field of the Machine Status Register (MSR)
as stored in ``CR0``.

.. code-block:: c++

  unsigned long __builtin_ttest (void)

This built-in can be used to determine the current transaction state
using the following code example:

.. code-block:: c++

  #include <htmintrin.h>

  unsigned char tx_state = _HTM_STATE (__builtin_ttest ());

  if (tx_state == _HTM_TRANSACTIONAL)
    {
      /* Code to use in transactional state.  */
    }
  else if (tx_state == _HTM_NONTRANSACTIONAL)
    {
      /* Code to use in non-transactional state.  */
    }
  else if (tx_state == _HTM_SUSPENDED)
    {
      /* Code to use in transaction suspended state.  */
    }

PowerPC HTM High Level Inline Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following high level HTM interface is made available by including
``<htmxlintrin.h>`` and using :option:`-mhtm` or :option:`-mcpu=CPU`
where CPU is 'power8' or later.  This interface is common between PowerPC
and S/390, allowing users to write one HTM source implementation that
can be compiled and executed on either system.

.. code-block:: c++

  long __TM_simple_begin (void);
  long __TM_begin (void* const TM_buff);
  long __TM_end (void);
  void __TM_abort (void);
  void __TM_named_abort (unsigned char const code);
  void __TM_resume (void);
  void __TM_suspend (void);

  long __TM_is_user_abort (void* const TM_buff);
  long __TM_is_named_user_abort (void* const TM_buff, unsigned char *code);
  long __TM_is_illegal (void* const TM_buff);
  long __TM_is_footprint_exceeded (void* const TM_buff);
  long __TM_nesting_depth (void* const TM_buff);
  long __TM_is_nested_too_deep(void* const TM_buff);
  long __TM_is_conflict(void* const TM_buff);
  long __TM_is_failure_persistent(void* const TM_buff);
  long __TM_failure_address(void* const TM_buff);
  long long __TM_failure_code(void* const TM_buff);

Using these common set of HTM inline functions, we can create
a more portable version of the HTM example in the previous
section that will work on either PowerPC or S/390:

.. code-block:: c++

  #include <htmxlintrin.h>

  int num_retries = 10;
  TM_buff_type TM_buff;

  while (1)
    {
      if (__TM_begin (TM_buff) == _HTM_TBEGIN_STARTED)
        {
          /* Transaction State Initiated.  */
          if (is_locked (lock))
            __TM_abort ();
          ... transaction code...
          __TM_end ();
          break;
        }
      else
        {
          /* Transaction State Failed.  Use locks if the transaction
             failure is "persistent" or we've tried too many times.  */
          if (num_retries-- <= 0
              || __TM_is_failure_persistent (TM_buff))
            {
              acquire_lock (lock);
              ... non transactional fallback path...
              release_lock (lock);
              break;
            }
        }
    }