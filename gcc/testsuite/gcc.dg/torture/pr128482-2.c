/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

__attribute__((noipa)) int f (int n)
{
  int b = 20, d = 0, k = 0;
  for (;;)
    {
      d = ~b;
      if (++k >= n)
	break;
      b += -2147483647 - 1;
    }
  return d;
}
int main (void)
{
  if (f (2) != 2147483627)
    __builtin_abort ();
  return 0;
}
