/* { dg-do run } */

/* This test fails to run at -O1 and above.  This is caused by

   ebbeaf490c5 [PR rtl-optimization/120553] Improve selecting between constants based on sign bit test

   and has been fixed by

   56ca14c4c4f Fix invalid right shift count with recent ifcvt changes

 */

__attribute__ ((noipa))
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
