/* PR optimization/8599 */
/* { dg-do run } */
/* { dg-options "-O2 -funroll-loops" } */
/* { dg-options "-mcpu=k6 -O2 -funroll-loops" { target i?86-*-* } } */

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
