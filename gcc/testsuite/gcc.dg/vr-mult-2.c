/* Make sure that mul/subu is preferred over mtlo/msac on targets that
   support both.  */
/* { dg-do compile { target mips*-*-* } } */
/* { dg-options "-O2" } */
#if defined (_MIPS_ARCH_VR5400) || defined (_MIPS_ARCH_VR5500)
int f (int a, int b, int c) { return a - b * c; }
#else
void f () { asm volatile ("mul/subu"); }
#endif
/* { dg-final { scan-assembler "mul.*subu" } } */
