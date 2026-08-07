/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_mve_ok } */
/* This is a duplicate of cmse-18.c, targetting arm_v8_1m_main_mve, to make
   sure FPCXT is enabled.  */
/* { dg-options "-mcmse -fdump-rtl-final" } */
/* { dg-add-options arm_arch_v8_1m_main_mve } */

typedef void (*f)(int) __attribute__((cmse_nonsecure_call));

void bar(f func, int a)
{
  func(a);
}

/* { dg-final { scan-rtl-dump "call \\\(mem:SI \\\(reg/f:SI \[0-7] r\[0-7\]" "final" } } */
/* { dg-final { scan-rtl-dump "UNSPEC_NONSECURE_MEM" "final" } } */
