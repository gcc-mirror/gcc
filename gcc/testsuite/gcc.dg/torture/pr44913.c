/* { dg-do run } */

void __attribute__((noinline,noclone))
foo (int *a, int n)
{
  int *lasta = a + n;
  for (; a != lasta; a++)
    {
      *a *= 2;
      a[1] = a[-1] + a[-2];
    }
}
extern void abort (void);
int main()
{
  int a[16] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
  int r[16] = { 1, 2, 6, 6, 16, 24, 44, 80, 136, 248, 432, 768, 1360, 2400, 4256, 3760 };
  unsigned i;
  foo (&a[2], 13);
  for (i = 0; i < 8; ++i)
    if (a[i] != r[i])
      abort ();
  return 0;
}
