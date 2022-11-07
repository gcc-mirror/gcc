..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _fr-v-built-in-functions:

FR-V Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^

GCC provides many FR-V-specific built-in functions.  In general,
these functions are intended to be compatible with those described
by FR-V Family, Softune C/C++ Compiler Manual (V6), Fujitsu
Semiconductor.  The two exceptions are ``__MDUNPACKH`` and
``__MBTOHE``, the GCC forms of which pass 128-bit values by
pointer rather than by value.

Most of the functions are named after specific FR-V instructions.
Such functions are said to be 'directly mapped' and are summarized
here in tabular form.

.. toctree::
  :maxdepth: 2


.. _argument-types:

Argument Types
~~~~~~~~~~~~~~

The arguments to the built-in functions can be divided into three groups:
register numbers, compile-time constants and run-time values.  In order
to make this classification clear at a glance, the arguments and return
values are given the following pseudo types:

.. list-table::
   :header-rows: 1

   * - Pseudo type
     - Real C type
     - Constant?
     - Description

   * - ``uh``
     - ``unsigned short``
     - No
     - an unsigned halfword
   * - ``uw1``
     - ``unsigned int``
     - No
     - an unsigned word
   * - ``sw1``
     - ``int``
     - No
     - a signed word
   * - ``uw2``
     - ``unsigned long long``
     - No
     - an unsigned doubleword
   * - ``sw2``
     - ``long long``
     - No
     - a signed doubleword
   * - ``const``
     - ``int``
     - Yes
     - an integer constant
   * - ``acc``
     - ``int``
     - Yes
     - an ACC register number
   * - ``iacc``
     - ``int``
     - Yes
     - an IACC register number

These pseudo types are not defined by GCC, they are simply a notational
convenience used in this manual.

Arguments of type ``uh``, ``uw1``, ``sw1``, ``uw2``
and ``sw2`` are evaluated at run time.  They correspond to
register operands in the underlying FR-V instructions.

``const`` arguments represent immediate operands in the underlying
FR-V instructions.  They must be compile-time constants.

``acc`` arguments are evaluated at compile time and specify the number
of an accumulator register.  For example, an ``acc`` argument of 2
selects the ACC2 register.

``iacc`` arguments are similar to ``acc`` arguments but specify the
number of an IACC register.  See see :ref:`other-builtins`
for more details.

.. _directly-mapped-integer-functions:

Directly-Mapped Integer Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The functions listed below map directly to FR-V I-type instructions.

.. list-table::
   :header-rows: 1

   * - Function prototype
     - Example usage
     - Assembly output

   * - ``sw1 __ADDSS (sw1, sw1)``
     - ``c = __ADDSS (a, b)``
     - ``ADDSS a,b,c``
   * - ``sw1 __SCAN (sw1, sw1)``
     - ``c = __SCAN (a, b)``
     - ``SCAN a,b,c``
   * - ``sw1 __SCUTSS (sw1)``
     - ``b = __SCUTSS (a)``
     - ``SCUTSS a,b``
   * - ``sw1 __SLASS (sw1, sw1)``
     - ``c = __SLASS (a, b)``
     - ``SLASS a,b,c``
   * - ``void __SMASS (sw1, sw1)``
     - ``__SMASS (a, b)``
     - ``SMASS a,b``
   * - ``void __SMSSS (sw1, sw1)``
     - ``__SMSSS (a, b)``
     - ``SMSSS a,b``
   * - ``void __SMU (sw1, sw1)``
     - ``__SMU (a, b)``
     - ``SMU a,b``
   * - ``sw2 __SMUL (sw1, sw1)``
     - ``c = __SMUL (a, b)``
     - ``SMUL a,b,c``
   * - ``sw1 __SUBSS (sw1, sw1)``
     - ``c = __SUBSS (a, b)``
     - ``SUBSS a,b,c``
   * - ``uw2 __UMUL (uw1, uw1)``
     - ``c = __UMUL (a, b)``
     - ``UMUL a,b,c``

.. _directly-mapped-media-functions:

Directly-Mapped Media Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The functions listed below map directly to FR-V M-type instructions.

.. list-table::
   :header-rows: 1

   * - Function prototype
     - Example usage
     - Assembly output

   * - ``uw1 __MABSHS (sw1)``
     - ``b = __MABSHS (a)``
     - ``MABSHS a,b``
   * - ``void __MADDACCS (acc, acc)``
     - ``__MADDACCS (b, a)``
     - ``MADDACCS a,b``
   * - ``sw1 __MADDHSS (sw1, sw1)``
     - ``c = __MADDHSS (a, b)``
     - ``MADDHSS a,b,c``
   * - ``uw1 __MADDHUS (uw1, uw1)``
     - ``c = __MADDHUS (a, b)``
     - ``MADDHUS a,b,c``
   * - ``uw1 __MAND (uw1, uw1)``
     - ``c = __MAND (a, b)``
     - ``MAND a,b,c``
   * - ``void __MASACCS (acc, acc)``
     - ``__MASACCS (b, a)``
     - ``MASACCS a,b``
   * - ``uw1 __MAVEH (uw1, uw1)``
     - ``c = __MAVEH (a, b)``
     - ``MAVEH a,b,c``
   * - ``uw2 __MBTOH (uw1)``
     - ``b = __MBTOH (a)``
     - ``MBTOH a,b``
   * - ``void __MBTOHE (uw1 *, uw1)``
     - ``__MBTOHE (&b, a)``
     - ``MBTOHE a,b``
   * - ``void __MCLRACC (acc)``
     - ``__MCLRACC (a)``
     - ``MCLRACC a``
   * - ``void __MCLRACCA (void)``
     - ``__MCLRACCA ()``
     - ``MCLRACCA``
   * - ``uw1 __Mcop1 (uw1, uw1)``
     - ``c = __Mcop1 (a, b)``
     - ``Mcop1 a,b,c``
   * - ``uw1 __Mcop2 (uw1, uw1)``
     - ``c = __Mcop2 (a, b)``
     - ``Mcop2 a,b,c``
   * - ``uw1 __MCPLHI (uw2, const)``
     - ``c = __MCPLHI (a, b)``
     - ``MCPLHI a,#b,c``
   * - ``uw1 __MCPLI (uw2, const)``
     - ``c = __MCPLI (a, b)``
     - ``MCPLI a,#b,c``
   * - ``void __MCPXIS (acc, sw1, sw1)``
     - ``__MCPXIS (c, a, b)``
     - ``MCPXIS a,b,c``
   * - ``void __MCPXIU (acc, uw1, uw1)``
     - ``__MCPXIU (c, a, b)``
     - ``MCPXIU a,b,c``
   * - ``void __MCPXRS (acc, sw1, sw1)``
     - ``__MCPXRS (c, a, b)``
     - ``MCPXRS a,b,c``
   * - ``void __MCPXRU (acc, uw1, uw1)``
     - ``__MCPXRU (c, a, b)``
     - ``MCPXRU a,b,c``
   * - ``uw1 __MCUT (acc, uw1)``
     - ``c = __MCUT (a, b)``
     - ``MCUT a,b,c``
   * - ``uw1 __MCUTSS (acc, sw1)``
     - ``c = __MCUTSS (a, b)``
     - ``MCUTSS a,b,c``
   * - ``void __MDADDACCS (acc, acc)``
     - ``__MDADDACCS (b, a)``
     - ``MDADDACCS a,b``
   * - ``void __MDASACCS (acc, acc)``
     - ``__MDASACCS (b, a)``
     - ``MDASACCS a,b``
   * - ``uw2 __MDCUTSSI (acc, const)``
     - ``c = __MDCUTSSI (a, b)``
     - ``MDCUTSSI a,#b,c``
   * - ``uw2 __MDPACKH (uw2, uw2)``
     - ``c = __MDPACKH (a, b)``
     - ``MDPACKH a,b,c``
   * - ``uw2 __MDROTLI (uw2, const)``
     - ``c = __MDROTLI (a, b)``
     - ``MDROTLI a,#b,c``
   * - ``void __MDSUBACCS (acc, acc)``
     - ``__MDSUBACCS (b, a)``
     - ``MDSUBACCS a,b``
   * - ``void __MDUNPACKH (uw1 *, uw2)``
     - ``__MDUNPACKH (&b, a)``
     - ``MDUNPACKH a,b``
   * - ``uw2 __MEXPDHD (uw1, const)``
     - ``c = __MEXPDHD (a, b)``
     - ``MEXPDHD a,#b,c``
   * - ``uw1 __MEXPDHW (uw1, const)``
     - ``c = __MEXPDHW (a, b)``
     - ``MEXPDHW a,#b,c``
   * - ``uw1 __MHDSETH (uw1, const)``
     - ``c = __MHDSETH (a, b)``
     - ``MHDSETH a,#b,c``
   * - ``sw1 __MHDSETS (const)``
     - ``b = __MHDSETS (a)``
     - ``MHDSETS #a,b``
   * - ``uw1 __MHSETHIH (uw1, const)``
     - ``b = __MHSETHIH (b, a)``
     - ``MHSETHIH #a,b``
   * - ``sw1 __MHSETHIS (sw1, const)``
     - ``b = __MHSETHIS (b, a)``
     - ``MHSETHIS #a,b``
   * - ``uw1 __MHSETLOH (uw1, const)``
     - ``b = __MHSETLOH (b, a)``
     - ``MHSETLOH #a,b``
   * - ``sw1 __MHSETLOS (sw1, const)``
     - ``b = __MHSETLOS (b, a)``
     - ``MHSETLOS #a,b``
   * - ``uw1 __MHTOB (uw2)``
     - ``b = __MHTOB (a)``
     - ``MHTOB a,b``
   * - ``void __MMACHS (acc, sw1, sw1)``
     - ``__MMACHS (c, a, b)``
     - ``MMACHS a,b,c``
   * - ``void __MMACHU (acc, uw1, uw1)``
     - ``__MMACHU (c, a, b)``
     - ``MMACHU a,b,c``
   * - ``void __MMRDHS (acc, sw1, sw1)``
     - ``__MMRDHS (c, a, b)``
     - ``MMRDHS a,b,c``
   * - ``void __MMRDHU (acc, uw1, uw1)``
     - ``__MMRDHU (c, a, b)``
     - ``MMRDHU a,b,c``
   * - ``void __MMULHS (acc, sw1, sw1)``
     - ``__MMULHS (c, a, b)``
     - ``MMULHS a,b,c``
   * - ``void __MMULHU (acc, uw1, uw1)``
     - ``__MMULHU (c, a, b)``
     - ``MMULHU a,b,c``
   * - ``void __MMULXHS (acc, sw1, sw1)``
     - ``__MMULXHS (c, a, b)``
     - ``MMULXHS a,b,c``
   * - ``void __MMULXHU (acc, uw1, uw1)``
     - ``__MMULXHU (c, a, b)``
     - ``MMULXHU a,b,c``
   * - ``uw1 __MNOT (uw1)``
     - ``b = __MNOT (a)``
     - ``MNOT a,b``
   * - ``uw1 __MOR (uw1, uw1)``
     - ``c = __MOR (a, b)``
     - ``MOR a,b,c``
   * - ``uw1 __MPACKH (uh, uh)``
     - ``c = __MPACKH (a, b)``
     - ``MPACKH a,b,c``
   * - ``sw2 __MQADDHSS (sw2, sw2)``
     - ``c = __MQADDHSS (a, b)``
     - ``MQADDHSS a,b,c``
   * - ``uw2 __MQADDHUS (uw2, uw2)``
     - ``c = __MQADDHUS (a, b)``
     - ``MQADDHUS a,b,c``
   * - ``void __MQCPXIS (acc, sw2, sw2)``
     - ``__MQCPXIS (c, a, b)``
     - ``MQCPXIS a,b,c``
   * - ``void __MQCPXIU (acc, uw2, uw2)``
     - ``__MQCPXIU (c, a, b)``
     - ``MQCPXIU a,b,c``
   * - ``void __MQCPXRS (acc, sw2, sw2)``
     - ``__MQCPXRS (c, a, b)``
     - ``MQCPXRS a,b,c``
   * - ``void __MQCPXRU (acc, uw2, uw2)``
     - ``__MQCPXRU (c, a, b)``
     - ``MQCPXRU a,b,c``
   * - ``sw2 __MQLCLRHS (sw2, sw2)``
     - ``c = __MQLCLRHS (a, b)``
     - ``MQLCLRHS a,b,c``
   * - ``sw2 __MQLMTHS (sw2, sw2)``
     - ``c = __MQLMTHS (a, b)``
     - ``MQLMTHS a,b,c``
   * - ``void __MQMACHS (acc, sw2, sw2)``
     - ``__MQMACHS (c, a, b)``
     - ``MQMACHS a,b,c``
   * - ``void __MQMACHU (acc, uw2, uw2)``
     - ``__MQMACHU (c, a, b)``
     - ``MQMACHU a,b,c``
   * - ``void __MQMACXHS (acc, sw2, sw2)``
     - ``__MQMACXHS (c, a, b)``
     - ``MQMACXHS a,b,c``
   * - ``void __MQMULHS (acc, sw2, sw2)``
     - ``__MQMULHS (c, a, b)``
     - ``MQMULHS a,b,c``
   * - ``void __MQMULHU (acc, uw2, uw2)``
     - ``__MQMULHU (c, a, b)``
     - ``MQMULHU a,b,c``
   * - ``void __MQMULXHS (acc, sw2, sw2)``
     - ``__MQMULXHS (c, a, b)``
     - ``MQMULXHS a,b,c``
   * - ``void __MQMULXHU (acc, uw2, uw2)``
     - ``__MQMULXHU (c, a, b)``
     - ``MQMULXHU a,b,c``
   * - ``sw2 __MQSATHS (sw2, sw2)``
     - ``c = __MQSATHS (a, b)``
     - ``MQSATHS a,b,c``
   * - ``uw2 __MQSLLHI (uw2, int)``
     - ``c = __MQSLLHI (a, b)``
     - ``MQSLLHI a,b,c``
   * - ``sw2 __MQSRAHI (sw2, int)``
     - ``c = __MQSRAHI (a, b)``
     - ``MQSRAHI a,b,c``
   * - ``sw2 __MQSUBHSS (sw2, sw2)``
     - ``c = __MQSUBHSS (a, b)``
     - ``MQSUBHSS a,b,c``
   * - ``uw2 __MQSUBHUS (uw2, uw2)``
     - ``c = __MQSUBHUS (a, b)``
     - ``MQSUBHUS a,b,c``
   * - ``void __MQXMACHS (acc, sw2, sw2)``
     - ``__MQXMACHS (c, a, b)``
     - ``MQXMACHS a,b,c``
   * - ``void __MQXMACXHS (acc, sw2, sw2)``
     - ``__MQXMACXHS (c, a, b)``
     - ``MQXMACXHS a,b,c``
   * - ``uw1 __MRDACC (acc)``
     - ``b = __MRDACC (a)``
     - ``MRDACC a,b``
   * - ``uw1 __MRDACCG (acc)``
     - ``b = __MRDACCG (a)``
     - ``MRDACCG a,b``
   * - ``uw1 __MROTLI (uw1, const)``
     - ``c = __MROTLI (a, b)``
     - ``MROTLI a,#b,c``
   * - ``uw1 __MROTRI (uw1, const)``
     - ``c = __MROTRI (a, b)``
     - ``MROTRI a,#b,c``
   * - ``sw1 __MSATHS (sw1, sw1)``
     - ``c = __MSATHS (a, b)``
     - ``MSATHS a,b,c``
   * - ``uw1 __MSATHU (uw1, uw1)``
     - ``c = __MSATHU (a, b)``
     - ``MSATHU a,b,c``
   * - ``uw1 __MSLLHI (uw1, const)``
     - ``c = __MSLLHI (a, b)``
     - ``MSLLHI a,#b,c``
   * - ``sw1 __MSRAHI (sw1, const)``
     - ``c = __MSRAHI (a, b)``
     - ``MSRAHI a,#b,c``
   * - ``uw1 __MSRLHI (uw1, const)``
     - ``c = __MSRLHI (a, b)``
     - ``MSRLHI a,#b,c``
   * - ``void __MSUBACCS (acc, acc)``
     - ``__MSUBACCS (b, a)``
     - ``MSUBACCS a,b``
   * - ``sw1 __MSUBHSS (sw1, sw1)``
     - ``c = __MSUBHSS (a, b)``
     - ``MSUBHSS a,b,c``
   * - ``uw1 __MSUBHUS (uw1, uw1)``
     - ``c = __MSUBHUS (a, b)``
     - ``MSUBHUS a,b,c``
   * - ``void __MTRAP (void)``
     - ``__MTRAP ()``
     - ``MTRAP``
   * - ``uw2 __MUNPACKH (uw1)``
     - ``b = __MUNPACKH (a)``
     - ``MUNPACKH a,b``
   * - ``uw1 __MWCUT (uw2, uw1)``
     - ``c = __MWCUT (a, b)``
     - ``MWCUT a,b,c``
   * - ``void __MWTACC (acc, uw1)``
     - ``__MWTACC (b, a)``
     - ``MWTACC a,b``
   * - ``void __MWTACCG (acc, uw1)``
     - ``__MWTACCG (b, a)``
     - ``MWTACCG a,b``
   * - ``uw1 __MXOR (uw1, uw1)``
     - ``c = __MXOR (a, b)``
     - ``MXOR a,b,c``

.. _raw-read-write-functions:

Raw Read/Write Functions
~~~~~~~~~~~~~~~~~~~~~~~~

This sections describes built-in functions related to read and write
instructions to access memory.  These functions generate
``membar`` instructions to flush the I/O load and stores where
appropriate, as described in Fujitsu's manual described above.

.. code-block:: c++

  unsigned char __builtin_read8 (void *data);
  unsigned short __builtin_read16 (void *data);
  unsigned long __builtin_read32 (void *data);
  unsigned long long __builtin_read64 (void *data);
  void __builtin_write8 (void *data, unsigned char datum);
  void __builtin_write16 (void *data, unsigned short datum);
  void __builtin_write32 (void *data, unsigned long datum);
  void __builtin_write64 (void *data, unsigned long long datum);

Other Built-in Functions
~~~~~~~~~~~~~~~~~~~~~~~~

This section describes built-in functions that are not named after
a specific FR-V instruction.

.. function:: sw2 __IACCreadll (iacc reg)

  Return the full 64-bit value of IACC0.  The :samp:`{reg}` argument is reserved
  for future expansion and must be 0.

.. function:: sw1 __IACCreadl (iacc reg)

  Return the value of IACC0H if :samp:`{reg}` is 0 and IACC0L if :samp:`{reg}` is 1.
  Other values of :samp:`{reg}` are rejected as invalid.

.. function:: void __IACCsetll (iacc reg, sw2 x)

  Set the full 64-bit value of IACC0 to :samp:`{x}`.  The :samp:`{reg}` argument
  is reserved for future expansion and must be 0.

.. function:: void __IACCsetl (iacc reg, sw1 x)

  Set IACC0H to :samp:`{x}` if :samp:`{reg}` is 0 and IACC0L to :samp:`{x}` if :samp:`{reg}`
  is 1.  Other values of :samp:`{reg}` are rejected as invalid.

.. function:: void __data_prefetch0 (const void *x)

  Use the ``dcpl`` instruction to load the contents of address :samp:`{x}`
  into the data cache.

.. function:: void __data_prefetch (const void *x)

  Use the ``nldub`` instruction to load the contents of address :samp:`{x}`
  into the data cache.  The instruction is issued in slot I1.