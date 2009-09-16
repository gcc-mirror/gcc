/* { dg-do compile } */

/* This used to result in an ICE.  */

extern _Decimal64 x;
extern int i;

void
foo (void)
{
  if (x <= 2.0)		/* { dg-error "mix operands" } */
    i++;
}
