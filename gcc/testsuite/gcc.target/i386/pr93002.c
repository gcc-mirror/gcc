/* PR target/93002 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "cmp\[^\n\r]*-1" } } */

volatile int sink;

void
foo (void)
{
  unsigned i = 1000;
  while (i--)
    sink = i;
}

void
bar (void)
{
  int i = 2000;
  while (i--)
    sink = i;
}
