/* PR target/81622 */
/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

void
foo (void)
{
  __builtin_vec_ld (1, 2);	/* { dg-error "invalid parameter combination" } */
  __builtin_vec_cmpne (1, 2);	/* { dg-error "invalid parameter combination" } */
  __builtin_vec_st (1, 0, 5);	/* { dg-error "invalid parameter combination" } */
}
