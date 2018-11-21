/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z14" } */

/* The user alignment ends up in TYPE_ALIGN of the type of the
   VAR_DECL.  */

typedef int __attribute__((vector_size(16),aligned(4))) v4si_4;
typedef int __attribute__((vector_size(16),aligned(8))) v4si_8;
typedef int __attribute__((vector_size(16),aligned(16))) v4si_16;
typedef int __attribute__((vector_size(16),aligned(32))) v4si_32;

v4si_4 a4;
v4si_8 a8;
v4si_16 a16;
v4si_32 a32;

void
foo (v4si_8 a)
{
  a4 += a;  /* vl ...   vst ... */
  a8 += a;  /* vl ...,3 vst ...,3 */
  a16 += a; /* vl ...,4 vst ...,4 */
  a32 += a; /* vl ...,4 vst ...,4 */
}

/* { dg-final { scan-assembler-times "vl\t%v\[0-9\]*,\[0-9\]*\\(%r\[0-9\]*\\),3\n" 1 } } */
/* { dg-final { scan-assembler-times "vl\t%v\[0-9\]*,\[0-9\]*\\(%r\[0-9\]*\\),4\n" 2 } } */
/* { dg-final { scan-assembler-times "vst\t%v\[0-9\]*,\[0-9\]*\\(%r\[0-9\]*\\),3\n" 1 } } */
/* { dg-final { scan-assembler-times "vst\t%v\[0-9\]*,\[0-9\]*\\(%r\[0-9\]*\\),4" 2 } } */
