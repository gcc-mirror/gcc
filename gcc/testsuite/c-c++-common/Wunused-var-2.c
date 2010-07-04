/* { dg-do compile } */
/* { dg-options "-Wunused" } */

int
f1 (void)
{
  int c = ({
    int a;
    a = 1;
    a; });
  return c;
}

void
f2 (void)
{
  int f;
  f = 0;
  __asm__ __volatile__ ("" : "+r" (f));
}
