..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _powerpc-matrix-multiply-assist-built-in-functions:

PowerPC Matrix-Multiply Assist Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

ISA 3.1 of the PowerPC added new Matrix-Multiply Assist (MMA) instructions.
GCC provides support for these instructions through the following built-in
functions which are enabled with the ``-mmma`` option.  The vec_t type
below is defined to be a normal vector unsigned char type.  The uint2, uint4
and uint8 parameters are 2-bit, 4-bit and 8-bit unsigned integer constants
respectively.  The compiler will verify that they are constants and that
their values are within range.

The built-in functions supported are:

.. code-block:: c++

  void __builtin_mma_xvi4ger8 (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvi8ger4 (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvi16ger2 (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvi16ger2s (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf16ger2 (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvbf16ger2 (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf32ger (__vector_quad *, vec_t, vec_t);

  void __builtin_mma_xvi4ger8pp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvi8ger4pp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvi8ger4spp(__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvi16ger2pp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvi16ger2spp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf16ger2pp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf16ger2pn (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf16ger2np (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf16ger2nn (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvbf16ger2pp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvbf16ger2pn (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvbf16ger2np (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvbf16ger2nn (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf32gerpp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf32gerpn (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf32gernp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf32gernn (__vector_quad *, vec_t, vec_t);

  void __builtin_mma_pmxvi4ger8 (__vector_quad *, vec_t, vec_t, uint4, uint4, uint8);
  void __builtin_mma_pmxvi4ger8pp (__vector_quad *, vec_t, vec_t, uint4, uint4, uint8);

  void __builtin_mma_pmxvi8ger4 (__vector_quad *, vec_t, vec_t, uint4, uint4, uint4);
  void __builtin_mma_pmxvi8ger4pp (__vector_quad *, vec_t, vec_t, uint4, uint4, uint4);
  void __builtin_mma_pmxvi8ger4spp(__vector_quad *, vec_t, vec_t, uint4, uint4, uint4);

  void __builtin_mma_pmxvi16ger2 (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvi16ger2s (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvf16ger2 (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvbf16ger2 (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);

  void __builtin_mma_pmxvi16ger2pp (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvi16ger2spp (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvf16ger2pp (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvf16ger2pn (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvf16ger2np (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvf16ger2nn (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvbf16ger2pp (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvbf16ger2pn (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvbf16ger2np (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvbf16ger2nn (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);

  void __builtin_mma_pmxvf32ger (__vector_quad *, vec_t, vec_t, uint4, uint4);
  void __builtin_mma_pmxvf32gerpp (__vector_quad *, vec_t, vec_t, uint4, uint4);
  void __builtin_mma_pmxvf32gerpn (__vector_quad *, vec_t, vec_t, uint4, uint4);
  void __builtin_mma_pmxvf32gernp (__vector_quad *, vec_t, vec_t, uint4, uint4);
  void __builtin_mma_pmxvf32gernn (__vector_quad *, vec_t, vec_t, uint4, uint4);

  void __builtin_mma_xvf64ger (__vector_quad *, __vector_pair, vec_t);
  void __builtin_mma_xvf64gerpp (__vector_quad *, __vector_pair, vec_t);
  void __builtin_mma_xvf64gerpn (__vector_quad *, __vector_pair, vec_t);
  void __builtin_mma_xvf64gernp (__vector_quad *, __vector_pair, vec_t);
  void __builtin_mma_xvf64gernn (__vector_quad *, __vector_pair, vec_t);

  void __builtin_mma_pmxvf64ger (__vector_quad *, __vector_pair, vec_t, uint4, uint2);
  void __builtin_mma_pmxvf64gerpp (__vector_quad *, __vector_pair, vec_t, uint4, uint2);
  void __builtin_mma_pmxvf64gerpn (__vector_quad *, __vector_pair, vec_t, uint4, uint2);
  void __builtin_mma_pmxvf64gernp (__vector_quad *, __vector_pair, vec_t, uint4, uint2);
  void __builtin_mma_pmxvf64gernn (__vector_quad *, __vector_pair, vec_t, uint4, uint2);

  void __builtin_mma_xxmtacc (__vector_quad *);
  void __builtin_mma_xxmfacc (__vector_quad *);
  void __builtin_mma_xxsetaccz (__vector_quad *);

  void __builtin_mma_build_acc (__vector_quad *, vec_t, vec_t, vec_t, vec_t);
  void __builtin_mma_disassemble_acc (void *, __vector_quad *);

  void __builtin_vsx_build_pair (__vector_pair *, vec_t, vec_t);
  void __builtin_vsx_disassemble_pair (void *, __vector_pair *);

  vec_t __builtin_vsx_xvcvspbf16 (vec_t);
  vec_t __builtin_vsx_xvcvbf16spn (vec_t);

  __vector_pair __builtin_vsx_lxvp (size_t, __vector_pair *);
  void __builtin_vsx_stxvp (__vector_pair, size_t, __vector_pair *);
