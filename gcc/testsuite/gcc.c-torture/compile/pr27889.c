/* { dg-require-effective-target trampolines } */

void h(void (*)(void));
_Complex int g (void)
{
  _Complex int x;
  void f(void)
  {
     x = x + x;
  }
  h(f);
  return x;
}
