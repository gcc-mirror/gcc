/* Although ip is not available, we can make use of a hole in the
   argument list for the indirect address (r1 is unused).  */

/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_pacbti_ok } */
/* { dg-add-options arm_arch_v8_1m_main_pacbti } */
/* { dg-additional-options "-mbranch-protection=pac-ret+leaf -O2 -std=c99" } */

void fail(void (*f)(int, ...))
{
  f(1, 2LL);
}

/* { dg-final { scan-assembler "bx\tr1\t@ indirect register sibling call" } } */
