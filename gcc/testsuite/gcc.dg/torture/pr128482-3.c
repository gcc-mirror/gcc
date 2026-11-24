/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

__attribute__((noipa)) int f (int n)
{
  int b = 13, d = 0, k = 0;
top:
  d = -414612191 - b * 2;
  if (++k < n)
    {
      b += -1073741824;
      goto top;
    }
  return d;
}
int main (void)
{
  if (f (2) != 1732871431)
    __builtin_abort ();
  return 0;
}
