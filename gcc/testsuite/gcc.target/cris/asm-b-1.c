/* { dg-do assemble } */
/* { dg-options "-O2" } */

/* Checking that the "b" constraint is accepted, for all target variants.  */

long sys_ipc (void)
{
  long __gu_err = -14;
  long dummy_for_get_user_asm_64_;
  __asm__ __volatile__( "move.d [%1+],%0\n"
			: "=r" (__gu_err), "=b" (dummy_for_get_user_asm_64_)
			: "0" (__gu_err));

  return __gu_err;
}
