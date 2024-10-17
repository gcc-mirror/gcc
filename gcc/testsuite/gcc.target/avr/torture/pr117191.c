/* { dg-do run } */

typedef int __attribute__((mode(SI))) __attribute__((vector_size (16))) vecint;
typedef int __attribute__((mode(SI))) siint;

vecint i = { 150, 100, 150, 200 };
vecint j = { 10, 13, 20, 30 };
vecint k;

int main (void)
{
  k = i / j;
  /* k = {15, 7, 7, 6} */
  if (k[0] != 15 || k[1] != 7)
    __builtin_abort ();

  k = i & j;
  /* k = {2, 4, 20, 8} */
  if (k[1] != 4)
    __builtin_abort ();

  return 0;
}
