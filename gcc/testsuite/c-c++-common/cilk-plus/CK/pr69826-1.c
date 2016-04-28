/* { dg-do run } */
/* { dg-require-effective-target cilkplus_runtime } */
/* { dg-options "-fcilkplus" } */
/* { dg-additional-options "-std=gnu99" { target c } } */

#define GRAINSIZE 2

int
main ()
{
  int a[64];
  #pragma cilk grainsize=GRAINSIZE
  _Cilk_for (int i = 0; i < 64; i++)
    a[i] = 0;
  #pragma cilk grainsize =GRAINSIZE
  _Cilk_for (int i = 0; i < 64; i++)
    a[i]++;
  #pragma cilk grainsize = GRAINSIZE
  _Cilk_for (int i = 0; i < 64; i++)
    a[i]++;
  for (int i = 0; i < 64; i++)
    if (a[i] != 2)
      __builtin_abort ();
  return 0;
}
