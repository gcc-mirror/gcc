/* Test visibility attribute.  */
/* { dg-do link { target ia64*-*-linux* } } */
/* { dg-options "-O2 -fpic" } */

int foo (int x);
int bar (int x) __asm__ ("foo") __attribute__ ((visibility ("hidden")));
int bar (int x)
{
  return x;
}

int main ()
{
  return 0;
}
