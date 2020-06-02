/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* The user alignment ends up in DECL_ALIGN of the VAR_DECL and is
   currently ignored if it is smaller than the alignment of the type.
   In this testcase an alignment hint ",3" is emitted also for
   accessing a4 which is wrong.
   Hence this testcase currently fails:
   https://gcc.gnu.org/bugzilla/show_bug.cgi?id=88085 */

typedef int __attribute__((vector_size(16))) v4si;

v4si a4 __attribute__((aligned(4)));
v4si a8 __attribute__((aligned(8)));
v4si a16 __attribute__((aligned(16)));
v4si a32 __attribute__((aligned(32)));

void
foo (v4si a)
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
