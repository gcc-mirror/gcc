/* PR optimization/8599 */
/* { dg-do run } */
/* { dg-options "-O2 -funroll-loops -fdump-rtl-loop2_unroll-details-blocks" } */
/* { dg-options "-mtune=k6 -O2 -funroll-loops -fdump-rtl-loop2_unroll-details-blocks" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */


extern void abort (void);

int array[6] = { 1,2,3,4,5,6 };

void foo()
{
  int i;

  for (i = 0; i < 5; i++)
    array[i] = 0;
}

int main()
{
  foo();
  if (array[0] || array [1] || array[2] || array[3] || array[4])
    abort ();
  if (array[5] != 6)
    abort ();
  return 0;
}
/* { dg-final { scan-rtl-dump-not "Invalid sum" "loop2_unroll" } } */
