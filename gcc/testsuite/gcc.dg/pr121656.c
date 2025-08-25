/* { dg-do run } */
/* { dg-options "-O1" } */

__attribute__ ((weak))
void
foo (int b)
{
  if (b != 3)
    __builtin_abort ();
}

int a;
int
main ()
{
  int b = 0;
  if (a >= 0)
    b += 3;
  foo (b);
  return 0;
}
