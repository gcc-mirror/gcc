/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8m_main_ok } */
/* Make sure FPCXT is not enabled.  */
/* { dg-options "-mcmse -fdump-rtl-final" } */
/* { dg-add-options arm_arch_v8m_main } */

typedef void (*f)(int) __attribute__((cmse_nonsecure_call));

void bar(f func, int a)
{
  func(a);
}

/* { dg-final { scan-rtl-dump "call \\\(mem:SI \\\(reg:SI 4 r4" "final" } } */
/* { dg-final { scan-rtl-dump "UNSPEC_NONSECURE_MEM" "final" } } */
