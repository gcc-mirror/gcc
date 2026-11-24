/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

__attribute__((noipa)) int f (int n)
{
  int b = 5, d = 0, k = 0;
  for (;;)
    {
      d = -10 - b;
      if (++k >= n)
	break;
      b += -2147483647 - 1;
    }
  return d;
}
int main (void)
{
  if (f (2) != 2147483633)
    __builtin_abort ();
  return 0;
}
