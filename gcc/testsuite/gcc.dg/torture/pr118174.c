/* { dg-do run } */

int __attribute__((noipa))
foo (signed char *p1, signed char *p2)
{
  int sum = 0;
  for (int i = 0; i < 32; i++)
    sum += __builtin_abs (p1[i] - p2[i]);
  return sum;
}

int
main()
{
  signed char a[32], b[32];
  for (int i = 0; i < 32; ++i)
    {
      a[i] = i;
      b[i] = 16 - i;
    }
  if (foo (a, b) != 624)
    __builtin_abort ();
  return 0;
}
