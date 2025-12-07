/* { dg-do compile } */

unsigned char f(int b)
{
  for (int a = 0; a < 10; a += 1)
    b = __builtin_ffs(b);
  return b;
}
