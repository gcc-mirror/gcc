/* { dg-do compile } */
/* { dg-options "-Os" } */

/* { dg-final { scan-assembler-times "swap" 4 } } */
/* { dg-final { scan-assembler-times "tst.w" 4 } } */
/* { dg-final { scan-assembler-not "cmp.l" } } */


unsigned int bar (void);
void
foo1 (void)
{
  unsigned int a = bar ();
  if (0x10000 <= a)
    bar ();
}


void
foo2 (void)
{
  unsigned int a = bar ();
  if (0x10000 > a)
    bar ();
}


void
foo3 (void)
{
  int a = bar ();
  if (0x10000 <= a)
    bar ();
}


void
foo4 (void)
{
  int a = bar ();
  if (0x10000 > a)
    bar ();
}





