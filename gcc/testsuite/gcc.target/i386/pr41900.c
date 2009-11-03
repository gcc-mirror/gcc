/* { dg-do run } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -fomit-frame-pointer -mpreferred-stack-boundary=2" } */

int main ()
{
  unsigned code = 0xc3;

  ((void (*)(void)) &code) ();
  return 0;
}
