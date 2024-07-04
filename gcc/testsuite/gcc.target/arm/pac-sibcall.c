/* If all call-clobbered general registers are live (r0-r3, ip), disable
   indirect tail-call for a PAC-enabled function.  */

/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_pacbti_ok } */
/* { dg-add-options arm_arch_v8_1m_main_pacbti } */
/* { dg-additional-options "-mbranch-protection=pac-ret+leaf -O2" } */

void fail(void (*f)(int, int, int, int))
{
  f(1, 2, 3, 4);
}

/* { dg-final { scan-assembler-not "bx\tip\t@ indirect register sibling call" } } */
