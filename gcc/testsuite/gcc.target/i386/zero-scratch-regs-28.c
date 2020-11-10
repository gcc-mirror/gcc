/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -mmmx -fzero-call-used-regs=all" } */
/* { dg-require-effective-target ia32 } */

typedef int __v2si __attribute__ ((vector_size (8)));

__v2si ret_mmx (void)
{
  return (__v2si) { 123, 345 };
}

/* { dg-final { scan-assembler "pxor\[ \t\]+%mm1, %mm1" } } */
/* { dg-final { scan-assembler "movq\[ \t\]+%mm1, %mm2" } } */
/* { dg-final { scan-assembler "movq\[ \t\]+%mm1, %mm3" } } */
/* { dg-final { scan-assembler "movq\[ \t\]+%mm1, %mm4" } } */
/* { dg-final { scan-assembler "movq\[ \t\]+%mm1, %mm5" } } */
/* { dg-final { scan-assembler "movq\[ \t\]+%mm1, %mm6" } } */
/* { dg-final { scan-assembler "movq\[ \t\]+%mm1, %mm7" } } */
