/* PR target/92469 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

void
foo (void)
{ 
  register int x asm ("frame");	/* { dg-error "register specified for 'x' is an internal GCC implementation detail" } */
  int y = x;
}

void
bar (void)
{ 
  register int x asm ("19");	/* { dg-error "register specified for 'x' is an internal GCC implementation detail" } */
  int y = x;
}

void
baz (void)
{ 
  register int x asm ("argp");	/* { dg-error "register specified for 'x' is an internal GCC implementation detail" } */
  int y = x;
}
