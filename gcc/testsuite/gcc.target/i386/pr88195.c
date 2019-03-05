/* PR target/88195 */
/* { dg-options "-mptwrite" } */

void
foo (void)
{
  __builtin_ia32_ptwrite64 (1);	/* { dg-warning "implicit declaration of function" "" { target ia32 } } */
}
