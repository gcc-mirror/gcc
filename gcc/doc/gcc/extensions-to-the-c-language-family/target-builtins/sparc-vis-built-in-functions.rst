..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _sparc-vis-built-in-functions:

SPARC VIS Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC supports SIMD operations on the SPARC using both the generic vector
extensions (see :ref:`vector-extensions`) as well as built-in functions for
the SPARC Visual Instruction Set (VIS).  When you use the :option:`-mvis`
switch, the VIS extension is exposed as the following built-in functions:

.. code-block:: c++

  typedef int v1si __attribute__ ((vector_size (4)));
  typedef int v2si __attribute__ ((vector_size (8)));
  typedef short v4hi __attribute__ ((vector_size (8)));
  typedef short v2hi __attribute__ ((vector_size (4)));
  typedef unsigned char v8qi __attribute__ ((vector_size (8)));
  typedef unsigned char v4qi __attribute__ ((vector_size (4)));

  void __builtin_vis_write_gsr (int64_t);
  int64_t __builtin_vis_read_gsr (void);

  void * __builtin_vis_alignaddr (void *, long);
  void * __builtin_vis_alignaddrl (void *, long);
  int64_t __builtin_vis_faligndatadi (int64_t, int64_t);
  v2si __builtin_vis_faligndatav2si (v2si, v2si);
  v4hi __builtin_vis_faligndatav4hi (v4si, v4si);
  v8qi __builtin_vis_faligndatav8qi (v8qi, v8qi);

  v4hi __builtin_vis_fexpand (v4qi);

  v4hi __builtin_vis_fmul8x16 (v4qi, v4hi);
  v4hi __builtin_vis_fmul8x16au (v4qi, v2hi);
  v4hi __builtin_vis_fmul8x16al (v4qi, v2hi);
  v4hi __builtin_vis_fmul8sux16 (v8qi, v4hi);
  v4hi __builtin_vis_fmul8ulx16 (v8qi, v4hi);
  v2si __builtin_vis_fmuld8sux16 (v4qi, v2hi);
  v2si __builtin_vis_fmuld8ulx16 (v4qi, v2hi);

  v4qi __builtin_vis_fpack16 (v4hi);
  v8qi __builtin_vis_fpack32 (v2si, v8qi);
  v2hi __builtin_vis_fpackfix (v2si);
  v8qi __builtin_vis_fpmerge (v4qi, v4qi);

  int64_t __builtin_vis_pdist (v8qi, v8qi, int64_t);

  long __builtin_vis_edge8 (void *, void *);
  long __builtin_vis_edge8l (void *, void *);
  long __builtin_vis_edge16 (void *, void *);
  long __builtin_vis_edge16l (void *, void *);
  long __builtin_vis_edge32 (void *, void *);
  long __builtin_vis_edge32l (void *, void *);

  long __builtin_vis_fcmple16 (v4hi, v4hi);
  long __builtin_vis_fcmple32 (v2si, v2si);
  long __builtin_vis_fcmpne16 (v4hi, v4hi);
  long __builtin_vis_fcmpne32 (v2si, v2si);
  long __builtin_vis_fcmpgt16 (v4hi, v4hi);
  long __builtin_vis_fcmpgt32 (v2si, v2si);
  long __builtin_vis_fcmpeq16 (v4hi, v4hi);
  long __builtin_vis_fcmpeq32 (v2si, v2si);

  v4hi __builtin_vis_fpadd16 (v4hi, v4hi);
  v2hi __builtin_vis_fpadd16s (v2hi, v2hi);
  v2si __builtin_vis_fpadd32 (v2si, v2si);
  v1si __builtin_vis_fpadd32s (v1si, v1si);
  v4hi __builtin_vis_fpsub16 (v4hi, v4hi);
  v2hi __builtin_vis_fpsub16s (v2hi, v2hi);
  v2si __builtin_vis_fpsub32 (v2si, v2si);
  v1si __builtin_vis_fpsub32s (v1si, v1si);

  long __builtin_vis_array8 (long, long);
  long __builtin_vis_array16 (long, long);
  long __builtin_vis_array32 (long, long);

When you use the :option:`-mvis2` switch, the VIS version 2.0 built-in
functions also become available:

.. code-block:: c++

  long __builtin_vis_bmask (long, long);
  int64_t __builtin_vis_bshuffledi (int64_t, int64_t);
  v2si __builtin_vis_bshufflev2si (v2si, v2si);
  v4hi __builtin_vis_bshufflev2si (v4hi, v4hi);
  v8qi __builtin_vis_bshufflev2si (v8qi, v8qi);

  long __builtin_vis_edge8n (void *, void *);
  long __builtin_vis_edge8ln (void *, void *);
  long __builtin_vis_edge16n (void *, void *);
  long __builtin_vis_edge16ln (void *, void *);
  long __builtin_vis_edge32n (void *, void *);
  long __builtin_vis_edge32ln (void *, void *);

When you use the :option:`-mvis3` switch, the VIS version 3.0 built-in
functions also become available:

.. code-block:: c++

  void __builtin_vis_cmask8 (long);
  void __builtin_vis_cmask16 (long);
  void __builtin_vis_cmask32 (long);

  v4hi __builtin_vis_fchksm16 (v4hi, v4hi);

  v4hi __builtin_vis_fsll16 (v4hi, v4hi);
  v4hi __builtin_vis_fslas16 (v4hi, v4hi);
  v4hi __builtin_vis_fsrl16 (v4hi, v4hi);
  v4hi __builtin_vis_fsra16 (v4hi, v4hi);
  v2si __builtin_vis_fsll16 (v2si, v2si);
  v2si __builtin_vis_fslas16 (v2si, v2si);
  v2si __builtin_vis_fsrl16 (v2si, v2si);
  v2si __builtin_vis_fsra16 (v2si, v2si);

  long __builtin_vis_pdistn (v8qi, v8qi);

  v4hi __builtin_vis_fmean16 (v4hi, v4hi);

  int64_t __builtin_vis_fpadd64 (int64_t, int64_t);
  int64_t __builtin_vis_fpsub64 (int64_t, int64_t);

  v4hi __builtin_vis_fpadds16 (v4hi, v4hi);
  v2hi __builtin_vis_fpadds16s (v2hi, v2hi);
  v4hi __builtin_vis_fpsubs16 (v4hi, v4hi);
  v2hi __builtin_vis_fpsubs16s (v2hi, v2hi);
  v2si __builtin_vis_fpadds32 (v2si, v2si);
  v1si __builtin_vis_fpadds32s (v1si, v1si);
  v2si __builtin_vis_fpsubs32 (v2si, v2si);
  v1si __builtin_vis_fpsubs32s (v1si, v1si);

  long __builtin_vis_fucmple8 (v8qi, v8qi);
  long __builtin_vis_fucmpne8 (v8qi, v8qi);
  long __builtin_vis_fucmpgt8 (v8qi, v8qi);
  long __builtin_vis_fucmpeq8 (v8qi, v8qi);

  float __builtin_vis_fhadds (float, float);
  double __builtin_vis_fhaddd (double, double);
  float __builtin_vis_fhsubs (float, float);
  double __builtin_vis_fhsubd (double, double);
  float __builtin_vis_fnhadds (float, float);
  double __builtin_vis_fnhaddd (double, double);

  int64_t __builtin_vis_umulxhi (int64_t, int64_t);
  int64_t __builtin_vis_xmulx (int64_t, int64_t);
  int64_t __builtin_vis_xmulxhi (int64_t, int64_t);

When you use the :option:`-mvis4` switch, the VIS version 4.0 built-in
functions also become available:

.. code-block:: c++

  v8qi __builtin_vis_fpadd8 (v8qi, v8qi);
  v8qi __builtin_vis_fpadds8 (v8qi, v8qi);
  v8qi __builtin_vis_fpaddus8 (v8qi, v8qi);
  v4hi __builtin_vis_fpaddus16 (v4hi, v4hi);

  v8qi __builtin_vis_fpsub8 (v8qi, v8qi);
  v8qi __builtin_vis_fpsubs8 (v8qi, v8qi);
  v8qi __builtin_vis_fpsubus8 (v8qi, v8qi);
  v4hi __builtin_vis_fpsubus16 (v4hi, v4hi);

  long __builtin_vis_fpcmple8 (v8qi, v8qi);
  long __builtin_vis_fpcmpgt8 (v8qi, v8qi);
  long __builtin_vis_fpcmpule16 (v4hi, v4hi);
  long __builtin_vis_fpcmpugt16 (v4hi, v4hi);
  long __builtin_vis_fpcmpule32 (v2si, v2si);
  long __builtin_vis_fpcmpugt32 (v2si, v2si);

  v8qi __builtin_vis_fpmax8 (v8qi, v8qi);
  v4hi __builtin_vis_fpmax16 (v4hi, v4hi);
  v2si __builtin_vis_fpmax32 (v2si, v2si);

  v8qi __builtin_vis_fpmaxu8 (v8qi, v8qi);
  v4hi __builtin_vis_fpmaxu16 (v4hi, v4hi);
  v2si __builtin_vis_fpmaxu32 (v2si, v2si);

  v8qi __builtin_vis_fpmin8 (v8qi, v8qi);
  v4hi __builtin_vis_fpmin16 (v4hi, v4hi);
  v2si __builtin_vis_fpmin32 (v2si, v2si);

  v8qi __builtin_vis_fpminu8 (v8qi, v8qi);
  v4hi __builtin_vis_fpminu16 (v4hi, v4hi);
  v2si __builtin_vis_fpminu32 (v2si, v2si);

When you use the :option:`-mvis4b` switch, the VIS version 4.0B
built-in functions also become available:

.. code-block:: c++

  v8qi __builtin_vis_dictunpack8 (double, int);
  v4hi __builtin_vis_dictunpack16 (double, int);
  v2si __builtin_vis_dictunpack32 (double, int);

  long __builtin_vis_fpcmple8shl (v8qi, v8qi, int);
  long __builtin_vis_fpcmpgt8shl (v8qi, v8qi, int);
  long __builtin_vis_fpcmpeq8shl (v8qi, v8qi, int);
  long __builtin_vis_fpcmpne8shl (v8qi, v8qi, int);

  long __builtin_vis_fpcmple16shl (v4hi, v4hi, int);
  long __builtin_vis_fpcmpgt16shl (v4hi, v4hi, int);
  long __builtin_vis_fpcmpeq16shl (v4hi, v4hi, int);
  long __builtin_vis_fpcmpne16shl (v4hi, v4hi, int);

  long __builtin_vis_fpcmple32shl (v2si, v2si, int);
  long __builtin_vis_fpcmpgt32shl (v2si, v2si, int);
  long __builtin_vis_fpcmpeq32shl (v2si, v2si, int);
  long __builtin_vis_fpcmpne32shl (v2si, v2si, int);

  long __builtin_vis_fpcmpule8shl (v8qi, v8qi, int);
  long __builtin_vis_fpcmpugt8shl (v8qi, v8qi, int);
  long __builtin_vis_fpcmpule16shl (v4hi, v4hi, int);
  long __builtin_vis_fpcmpugt16shl (v4hi, v4hi, int);
  long __builtin_vis_fpcmpule32shl (v2si, v2si, int);
  long __builtin_vis_fpcmpugt32shl (v2si, v2si, int);

  long __builtin_vis_fpcmpde8shl (v8qi, v8qi, int);
  long __builtin_vis_fpcmpde16shl (v4hi, v4hi, int);
  long __builtin_vis_fpcmpde32shl (v2si, v2si, int);

  long __builtin_vis_fpcmpur8shl (v8qi, v8qi, int);
  long __builtin_vis_fpcmpur16shl (v4hi, v4hi, int);
  long __builtin_vis_fpcmpur32shl (v2si, v2si, int);