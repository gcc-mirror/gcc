/* { dg-do run { target { powerpc*-*-* && vmx_hw } } } */
/* { dg-do compile { target { powerpc*-*-* && { ! vmx_hw } } } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -mabi=altivec -O2" } */

/* Check that "easy" AltiVec constants are correctly synthesized.  */

extern void abort (void);

typedef __attribute__ ((vector_size (16))) unsigned char v16qi;
typedef __attribute__ ((vector_size (16))) unsigned short v8hi;
typedef __attribute__ ((vector_size (16))) unsigned int v4si;

typedef __attribute__((aligned(16))) char c16[16];
typedef __attribute__((aligned(16))) short s8[8];
typedef __attribute__((aligned(16))) int i4[4];

#define V16QI(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16)	\
  v16qi v = {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16};	\
  static c16 w = {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16}; \
  check_v16qi (v, w);

#define V8HI(V1,V2,V3,V4,V5,V6,V7,V8)		\
  v8hi v = {V1,V2,V3,V4,V5,V6,V7,V8};		\
  static s8 w = {V1,V2,V3,V4,V5,V6,V7,V8};	\
  check_v8hi (v, w);

#define V4SI(V1,V2,V3,V4)	\
  v4si v = {V1,V2,V3,V4};	\
  static i4 w = {V1,V2,V3,V4};	\
  check_v4si (v, w);


/* Use three different check functions for each mode-instruction pair.
   The callers have no typecasting and no addressable vectors, to make
   the test more robust.  */

void __attribute__ ((noinline)) check_v16qi (v16qi v1, char *v2)
{
  if (memcmp (&v1, v2, 16))
    abort ();
}

void __attribute__ ((noinline)) check_v8hi (v8hi v1, short *v2)
{
  if (memcmp (&v1, v2, 16))
    abort ();
}

void __attribute__ ((noinline)) check_v4si (v4si v1, int *v2)
{
  if (memcmp (&v1, v2, 16))
    abort ();
}


/* V16QI tests.  */

void v16qi_vspltisb ()
{
  V16QI (15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15);
}

void v16qi_vspltisb_neg ()
{
  V16QI (-5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5);
}

void v16qi_vspltisb_addself ()
{
  V16QI (30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30);
}

void v16qi_vspltisb_neg_addself ()
{
  V16QI (-24, -24, -24, -24, -24, -24, -24, -24, -24, -24, -24, -24, -24, -24, -24, -24);
}

void v16qi_vspltish ()
{
  V16QI (15, 0, 15, 0, 15, 0, 15, 0, 15, 0, 15, 0, 15, 0, 15, 0);
}

void v16qi_vspltish_addself ()
{
  V16QI (30, 0, 30, 0, 30, 0, 30, 0, 30, 0, 30, 0, 30, 0, 30, 0);
}

void v16qi_vspltish_neg ()
{
  V16QI (-5, -1, -5, -1, -5, -1, -5, -1, -5, -1, -5, -1, -5, -1, -5, -1);
}

void v16qi_vspltisw ()
{
  V16QI (15, 0, 0, 0, 15, 0, 0, 0, 15, 0, 0, 0, 15, 0, 0, 0);
}

void v16qi_vspltisw_addself ()
{
  V16QI (30, 0, 0, 0, 30, 0, 0, 0, 30, 0, 0, 0, 30, 0, 0, 0);
}

void v16qi_vspltisw_neg ()
{
  V16QI (-5, -1, -1, -1, -5, -1, -1, -1, -5, -1, -1, -1, -5, -1, -1, -1);
}


/* V8HI tests. */

void v8hi_vspltisb ()
{
  V8HI (0x0F0F, 0x0F0F, 0x0F0F, 0x0F0F, 0x0F0F, 0x0F0F, 0x0F0F, 0x0F0F);
}

void v8hi_vspltisb_addself ()
{
  V8HI (0x1E1E, 0x1E1E, 0x1E1E, 0x1E1E, 0x1E1E, 0x1E1E, 0x1E1E, 0x1E1E);
}

void v8hi_vspltisb_neg ()
{
  V8HI (0xFBFB, 0xFBFB, 0xFBFB, 0xFBFB, 0xFBFB, 0xFBFB, 0xFBFB, 0xFBFB);
}

void v8hi_vspltish ()
{
  V8HI (15, 15, 15, 15, 15, 15, 15, 15);
}

void v8hi_vspltish_neg ()
{
  V8HI (-5, -5, -5, -5, -5, -5, -5, -5);
}

void v8hi_vspltish_addself ()
{
  V8HI (30, 30, 30, 30, 30, 30, 30, 30);
}

void v8hi_vspltish_neg_addself ()
{
  V8HI (-24, -24, -24, -24, -24, -24, -24, -24);
}

void v8hi_vspltisw ()
{
  V8HI (15, 0, 15, 0, 15, 0, 15, 0);
}

void v8hi_vspltisw_addself ()
{
  V8HI (30, 0, 30, 0, 30, 0, 30, 0);
}

void v8hi_vspltisw_neg ()
{
  V8HI (-5, -1, -5, -1, -5, -1, -5, -1);
}

/* V4SI tests. */

void v4si_vspltisb ()
{
  V4SI (0x0F0F0F0F, 0x0F0F0F0F, 0x0F0F0F0F, 0x0F0F0F0F);
}

void v4si_vspltisb_addself ()
{
  V4SI (0x1E1E1E1E, 0x1E1E1E1E, 0x1E1E1E1E, 0x1E1E1E1E);
}

void v4si_vspltisb_neg ()
{
  V4SI (0xFBFBFBFB, 0xFBFBFBFB, 0xFBFBFBFB, 0xFBFBFBFB);
}

void v4si_vspltish ()
{
  V4SI (0x000F000F, 0x000F000F, 0x000F000F, 0x000F000F);
}

void v4si_vspltish_addself ()
{
  V4SI (0x001E001E, 0x001E001E, 0x001E001E, 0x001E001E);
}

void v4si_vspltish_neg ()
{
  V4SI (0xFFFBFFFB, 0xFFFBFFFB, 0xFFFBFFFB, 0xFFFBFFFB);
}

void v4si_vspltisw ()
{
  V4SI (15, 15, 15, 15);
}

void v4si_vspltisw_neg ()
{
  V4SI (-5, -5, -5, -5);
}

void v4si_vspltisw_addself ()
{
  V4SI (30, 30, 30, 30);
}

void v4si_vspltisw_neg_addself ()
{
  V4SI (-24, -24, -24, -24);
}



int main ()
{
  v16qi_vspltisb ();
  v16qi_vspltisb_neg ();
  v16qi_vspltisb_addself ();
  v16qi_vspltisb_neg_addself ();
  v16qi_vspltish ();
  v16qi_vspltish_addself ();
  v16qi_vspltish_neg ();
  v16qi_vspltisw ();
  v16qi_vspltisw_addself ();
  v16qi_vspltisw_neg ();

  v8hi_vspltisb ();
  v8hi_vspltisb_addself ();
  v8hi_vspltisb_neg ();
  v8hi_vspltish ();
  v8hi_vspltish_neg ();
  v8hi_vspltish_addself ();
  v8hi_vspltish_neg_addself ();
  v8hi_vspltisw ();
  v8hi_vspltisw_addself ();
  v8hi_vspltisw_neg ();

  v4si_vspltisb ();
  v4si_vspltisb_addself ();
  v4si_vspltisb_neg ();
  v4si_vspltish ();
  v4si_vspltish_addself ();
  v4si_vspltish_neg ();
  v4si_vspltisw ();
  v4si_vspltisw_neg ();
  v4si_vspltisw_addself ();
  v4si_vspltisw_neg_addself ();
  return 0;
}

/* { dg-final { scan-assembler-not "lvx" { target { powerpc*le-*-* } } } } */
