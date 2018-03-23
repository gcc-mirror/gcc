/* { dg-do run } */

void __attribute__((noipa)) g(int p, int *out)
{
  int x = 0, y;
#pragma GCC ivdep
  for (int i = 0; i < 100; i++)
    {
      int &r = p ? x : y;
      r = 42;
      out[i] = x;
    }
}

int main()
{
  int out[100] = { 0 };
  g (1, out);
  if (out[0] != 42)
    __builtin_abort ();
  return 0;
}
