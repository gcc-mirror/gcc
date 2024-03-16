/* PR target/85511 */
/* { dg-do compile } */
/* { dg-options "-fpermissive -Wimplicit-function-declaration" } */

unsigned int
foo (void)
{
  return __builtin_ia32_readeflags_u32 ();	/* { dg-warning "implicit declaration of function" "" { target { ! ia32 } } } */
}

void
bar (unsigned int x)
{
  __builtin_ia32_writeeflags_u32 (x);		/* { dg-warning "implicit declaration of function" "" { target { ! ia32 } } } */
}
