/* Make sure that mul/addu is preferred over mtlo/macc on targets that
   support both.  */
/* { dg-do compile { target mips*-*-* } } */
/* { dg-options "-O2" } */
#if defined (_MIPS_ARCH_VR5400) || defined (_MIPS_ARCH_VR5500)
int f (int a, int b, int c) { return a + b * c; }
#else
void f () { asm volatile ("mul/addu"); }
#endif
/* { dg-final { scan-assembler "mul.*addu" } } */
