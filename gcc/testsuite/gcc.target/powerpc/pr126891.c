/* PR target/126891 */
/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target hard_dfp } */

/* Verify that __builtin_set_fpscr_drn does not ICE with an integer
   variable argument.  */

int main ()
{
  int a = 7;
  __builtin_set_fpscr_drn (a);
  return 0;
}
