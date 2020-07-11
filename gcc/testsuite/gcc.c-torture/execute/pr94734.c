/* PR tree-optimization/94734 */

__attribute__((noipa)) int
foo (int n)
{
  int arr[16], s = 0;
  for (int i = 0; i < n; i++)
    {
      if (i < 16)
	arr[i] = i;
    }
  for (int i = 0; i < 16; i++)
    s += arr[i];
  return s;
}

__attribute__((noipa)) int
bar (int n, int x, unsigned long y, unsigned long z)
{
  int arr[16], s = 0;
  arr[4] = 42;
  for (int i = 0; i < n; i++)
    {
      if (x == (i & 0x25))
	arr[y] = i;
    }
  return arr[z];
}

__attribute__((noipa)) int
baz (int n, int x, unsigned long z)
{
  int arr[16], s = 0;
  arr[12] = 42;
  for (int i = 0; i < n; i++)
    {
      if (x == (i & 0x25))
	arr[7] = i;
    }
  return arr[z];
}

int
main ()
{
  if (foo (10374) != 15 * 16 / 2)
    __builtin_abort ();
  if (bar (25, 0x25, (unsigned long) 0xdeadbeefbeefdeadULL, 4) != 42)
    __builtin_abort ();
  if (bar (25, 4, 15, 15) != 22)
    __builtin_abort ();
  if (baz (25, 0x25, 12) != 42)
    __builtin_abort ();
  if (baz (25, 4, 7) != 22)
    __builtin_abort ();
  if (baz (25, 4, 12) != 42)
    __builtin_abort ();
  return 0;
}
