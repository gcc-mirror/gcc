/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-O2 -Werror-implicit-function-declaration -mARC700 -msimd" } */

#define STEST1(name, rettype, op1)		\
  rettype test_ ## name                         \
  (void)					\
  {                                             \
    return __builtin_arc_ ## name (op1);	\
  }

#define STEST2(name, rettype, op1, op2)  \
  rettype test_ ## name                         \
  (void)					\
  {                                             \
    return __builtin_arc_ ## name (op1, op2);	\
  }

#define STEST3(name, rettype, op1, op2, op3)	\
  rettype test_ ## name                         \
  (void)					\
  {                                             \
    return __builtin_arc_ ## name (op1, op2, op3);	\
  }

#define STEST4(name, rettype, op1, op2, op3, op4)	\
  rettype test_ ## name					\
  (void)						\
  {							\
    return __builtin_arc_ ## name (op1, op2, op3, op4);	\
  }

typedef short v8hi __attribute__ ((vector_size (16)));

v8hi Va;
v8hi Vb;
v8hi Vc;
#define rlimm 0xf3eec0fe
#define Ic    0x02
#define Ib    0x02
#define u3    0x02
#define u6    0x1F
#define u8    0xB0

STEST2 ( vaddaw, v8hi, Vb, Vc)
STEST2 (  vaddw, v8hi, Vb, Vc)
STEST2 (   vavb, v8hi, Vb, Vc)
STEST2 (  vavrb, v8hi, Vb, Vc)
STEST2 ( vdifaw, v8hi, Vb, Vc)
STEST2 (  vdifw, v8hi, Vb, Vc)
STEST2 ( vmaxaw, v8hi, Vb, Vc)
STEST2 (  vmaxw, v8hi, Vb, Vc)
STEST2 ( vminaw, v8hi, Vb, Vc)
STEST2 (  vminw, v8hi, Vb, Vc)
STEST2 ( vmulaw, v8hi, Vb, Vc)
STEST2 (vmulfaw, v8hi, Vb, Vc)
STEST2 ( vmulfw, v8hi, Vb, Vc)
STEST2 (  vmulw, v8hi, Vb, Vc)
STEST2 ( vsubaw, v8hi, Vb, Vc)
STEST2 (  vsubw, v8hi, Vb, Vc)
STEST2 ( vsummw, v8hi, Vb, Vc)
STEST2 (   vand, v8hi, Vb, Vc)
STEST2 ( vandaw, v8hi, Vb, Vc)
STEST2 (   vbic, v8hi, Vb, Vc)
STEST2 ( vbicaw, v8hi, Vb, Vc)
STEST2 (    vor, v8hi, Vb, Vc)
STEST2 (   vxor, v8hi, Vb, Vc)
STEST2 ( vxoraw, v8hi, Vb, Vc)
STEST2 (   veqw, v8hi, Vb, Vc)
STEST2 (   vlew, v8hi, Vb, Vc)
STEST2 (   vltw, v8hi, Vb, Vc)
STEST2 (   vnew, v8hi, Vb, Vc)
STEST2 ( vmr1aw, v8hi, Vb, Vc)
STEST2 (  vmr1w, v8hi, Vb, Vc)
STEST2 ( vmr2aw, v8hi, Vb, Vc)
STEST2 (  vmr2w, v8hi, Vb, Vc)
STEST2 ( vmr3aw, v8hi, Vb, Vc)
STEST2 (  vmr3w, v8hi, Vb, Vc)
STEST2 ( vmr4aw, v8hi, Vb, Vc)
STEST2 (  vmr4w, v8hi, Vb, Vc)
STEST2 ( vmr5aw, v8hi, Vb, Vc)
STEST2 (  vmr5w, v8hi, Vb, Vc)
STEST2 ( vmr6aw, v8hi, Vb, Vc)
STEST2 (  vmr6w, v8hi, Vb, Vc)
STEST2 ( vmr7aw, v8hi, Vb, Vc)
STEST2 (  vmr7w, v8hi, Vb, Vc)
STEST2 (   vmrb, v8hi, Vb, Vc)
STEST2 ( vh264f, v8hi, Vb, Vc)
STEST2 (vh264ft, v8hi, Vb, Vc)
STEST2 (vh264fw, v8hi, Vb, Vc)
STEST2 (  vvc1f, v8hi, Vb, Vc)
STEST2 ( vvc1ft, v8hi, Vb, Vc)

STEST2 ( vbaddw, v8hi, Vb, rlimm)
STEST2 ( vbmaxw, v8hi, Vb, rlimm)
STEST2 ( vbminw, v8hi, Vb, rlimm)
STEST2 (vbmulaw, v8hi, Vb, rlimm)
STEST2 (vbmulfw, v8hi, Vb, rlimm)
STEST2 ( vbmulw, v8hi, Vb, rlimm)
STEST2 (vbrsubw, v8hi, Vb, rlimm)
STEST2 ( vbsubw, v8hi, Vb, rlimm)


/* Va, Vb, Ic instructions.  */
STEST2 ( vasrw, v8hi, Vb, Ic)
STEST2 (  vsr8, v8hi, Vb, Ic)
STEST2 (vsr8aw, v8hi, Vb, Ic)

/* Va, Vb, u6 instructions.  */
STEST2 (  vasrrwi, v8hi, Vb, u6)
STEST2 ( vasrsrwi, v8hi, Vb, u6)
STEST2 (   vasrwi, v8hi, Vb, u6)
STEST2 ( vasrpwbi, v8hi, Vb, u6)
STEST2 (vasrrpwbi, v8hi, Vb, u6)
STEST2 (  vsr8awi, v8hi, Vb, u6)
STEST2 (    vsr8i, v8hi, Vb, u6)

/* Va, Vb, u8 (simm) instructions.  */
STEST2 (  vmvaw, v8hi, Vb, u8)
STEST2 (   vmvw, v8hi, Vb, u8)
STEST2 (  vmvzw, v8hi, Vb, u8)
STEST2 (vd6tapf, v8hi, Vb, u8)

/* Va, rlimm, u8 (simm) instructions.  */
STEST2 (vmovaw, v8hi, rlimm, u8)
STEST2 ( vmovw, v8hi, rlimm, u8)
STEST2 (vmovzw, v8hi, rlimm, u8)

/* Va, Vb instructions.  */
STEST1 ( vabsaw, v8hi, Vb)
STEST1 (  vabsw, v8hi, Vb)
STEST1 (vaddsuw, v8hi, Vb)
STEST1 ( vsignw, v8hi, Vb)
STEST1 ( vexch1, v8hi, Vb)
STEST1 ( vexch2, v8hi, Vb)
STEST1 ( vexch4, v8hi, Vb)
STEST1 ( vupbaw, v8hi, Vb)
STEST1 (  vupbw, v8hi, Vb)
STEST1 (vupsbaw, v8hi, Vb)
STEST1 ( vupsbw, v8hi, Vb)

/* DIb, rlimm, rlimm instructions.  */
STEST2 (vdirun, void, rlimm, rlimm)
STEST2 (vdorun, void, rlimm, rlimm)

/* DIb, limm, rlimm instructions.  */
STEST2 (vdiwr, void, u3, rlimm)
STEST2 (vdowr, void, u3, rlimm)

/* rlimm instructions.  */
STEST1 (   vrec, void, rlimm)
STEST1 (   vrun, void, rlimm)
STEST1 (vrecrun, void, rlimm)
STEST1 (vendrec, void, rlimm)

/* Va, [Ib,u8] instructions.  */
STEST3  (vld32wh, v8hi, Vb, Ic, u8)
STEST3  (vld32wl, v8hi, Vb, Ic, u8)
STEST3  (  vld64, v8hi, Vb, Ic, u8)
STEST3  (  vld32, v8hi, Vb, Ic, u8)

STEST2  (vld64w, v8hi, Ib, u8)
STEST2  (vld128, v8hi, Ib, u8)

STEST3 (vst128, void, Va, Ib, u8)
STEST3 ( vst64, void, Va, Ib, u8)

/* Va, [Ib, u8] instructions.  */
STEST4 (vst16_n, void, Va, u3, Ib, u8)
STEST4 (vst32_n, void, Va, u3, Ib, u8)

STEST1 (vinti, void, u6)
