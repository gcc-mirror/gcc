/* Test whether -fverbose-asm works.  */
/* { dg-do compile } */
/* { dg-options "-fverbose-asm" } */

void foo (int *x)
{
  (*x)++;
}

int bar (int *y)
{
  int a, b;
  b = 10;
  a = 26;
  foo (&a);
  a += 10;
  foo (&a);
  *y--;
  return b;
}
   
int
main (int argc, char *argv [])
{
  bar (&argc);
  return 0;
}
