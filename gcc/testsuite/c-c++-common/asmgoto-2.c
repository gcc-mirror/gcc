/* { dg-do compile } */
/* { dg-options "" } */

void
foo (void)
{
  __label__ lab;
  int i = 0;
  asm goto ("" : : : : lab);
  asm goto ("" : "=r" (i) : : : lab);	/* { dg-error "expected" } */
  asm goto ("" : : : : );	/* { dg-error "expected" } */
  asm goto ("" : : : "memory");	/* { dg-error "expected" } */
  asm goto ("" : : : );		/* { dg-error "expected" } */
  asm goto ("" : : "r" (i));	/* { dg-error "expected" } */
  asm goto ("" : : );		/* { dg-error "expected" } */
  asm goto ("" : "=r" (i));	/* { dg-error "expected" } */
  asm goto ("" : );		/* { dg-error "expected" } */
  asm goto ("");		/* { dg-error "expected" } */
  lab:;
}
