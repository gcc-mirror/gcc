..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _altera-nios-ii-built-in-functions:

Altera Nios II Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the Altera Nios II
family of processors.

The following built-in functions are always available.  They
all generate the machine instruction that is part of the name.

.. code-block:: c++

  int __builtin_ldbio (volatile const void *);
  int __builtin_ldbuio (volatile const void *);
  int __builtin_ldhio (volatile const void *);
  int __builtin_ldhuio (volatile const void *);
  int __builtin_ldwio (volatile const void *);
  void __builtin_stbio (volatile void *, int);
  void __builtin_sthio (volatile void *, int);
  void __builtin_stwio (volatile void *, int);
  void __builtin_sync (void);
  int __builtin_rdctl (int);
  int __builtin_rdprs (int, int);
  void __builtin_wrctl (int, int);
  void __builtin_flushd (volatile void *);
  void __builtin_flushda (volatile void *);
  int __builtin_wrpie (int);
  void __builtin_eni (int);
  int __builtin_ldex (volatile const void *);
  int __builtin_stex (volatile void *, int);
  int __builtin_ldsex (volatile const void *);
  int __builtin_stsex (volatile void *, int);

The following built-in functions are always available.  They
all generate a Nios II Custom Instruction. The name of the
function represents the types that the function takes and
returns. The letter before the ``n`` is the return type
or void if absent. The ``n`` represents the first parameter
to all the custom instructions, the custom instruction number.
The two letters after the ``n`` represent the up to two
parameters to the function.

The letters represent the following data types:

``<no letter>``
  ``void`` for return type and no parameter for parameter types.

``i``
  ``int`` for return type and parameter type

``f``
  ``float`` for return type and parameter type

``p``
  ``void *`` for return type and parameter type

  And the function names are:

.. code-block:: c++

  void __builtin_custom_n (void);
  void __builtin_custom_ni (int);
  void __builtin_custom_nf (float);
  void __builtin_custom_np (void *);
  void __builtin_custom_nii (int, int);
  void __builtin_custom_nif (int, float);
  void __builtin_custom_nip (int, void *);
  void __builtin_custom_nfi (float, int);
  void __builtin_custom_nff (float, float);
  void __builtin_custom_nfp (float, void *);
  void __builtin_custom_npi (void *, int);
  void __builtin_custom_npf (void *, float);
  void __builtin_custom_npp (void *, void *);
  int __builtin_custom_in (void);
  int __builtin_custom_ini (int);
  int __builtin_custom_inf (float);
  int __builtin_custom_inp (void *);
  int __builtin_custom_inii (int, int);
  int __builtin_custom_inif (int, float);
  int __builtin_custom_inip (int, void *);
  int __builtin_custom_infi (float, int);
  int __builtin_custom_inff (float, float);
  int __builtin_custom_infp (float, void *);
  int __builtin_custom_inpi (void *, int);
  int __builtin_custom_inpf (void *, float);
  int __builtin_custom_inpp (void *, void *);
  float __builtin_custom_fn (void);
  float __builtin_custom_fni (int);
  float __builtin_custom_fnf (float);
  float __builtin_custom_fnp (void *);
  float __builtin_custom_fnii (int, int);
  float __builtin_custom_fnif (int, float);
  float __builtin_custom_fnip (int, void *);
  float __builtin_custom_fnfi (float, int);
  float __builtin_custom_fnff (float, float);
  float __builtin_custom_fnfp (float, void *);
  float __builtin_custom_fnpi (void *, int);
  float __builtin_custom_fnpf (void *, float);
  float __builtin_custom_fnpp (void *, void *);
  void * __builtin_custom_pn (void);
  void * __builtin_custom_pni (int);
  void * __builtin_custom_pnf (float);
  void * __builtin_custom_pnp (void *);
  void * __builtin_custom_pnii (int, int);
  void * __builtin_custom_pnif (int, float);
  void * __builtin_custom_pnip (int, void *);
  void * __builtin_custom_pnfi (float, int);
  void * __builtin_custom_pnff (float, float);
  void * __builtin_custom_pnfp (float, void *);
  void * __builtin_custom_pnpi (void *, int);
  void * __builtin_custom_pnpf (void *, float);
  void * __builtin_custom_pnpp (void *, void *);