/* { dg-do run } */
/* { dg-options "-ftree-vectorize" } */

float val[256];
float x;
void __attribute__((noinline,noclone))
foo(int len, int beg)
{
  int i;
  for (i = len - 1; i >= beg; --i)
    x += val[i] * 2;
}
void __attribute__((noinline,noclone))
bar(void)
{
  int i;
  for (i = 255; i >= 0; --i)
    x += val[i] * 2;
  for (i = 254; i >= 0; --i)
    x += val[i] * 2;
  for (i = 253; i >= 0; --i)
    x += val[i] * 2;
  for (i = 252; i >= 0; --i)
    x += val[i] * 2;
}
float y[256];
void __attribute__((noinline,noclone))
foobar(void)
{
  int i;
  for (i = 0; i < 252; ++i)
    {
      float l = 0;
      l += val[255 - i] * 2;
      l += val[254 - i] * 2;
      l += val[253 - i] * 2;
      l += val[252 - i] * 2;
      y[i] = l;
    }
}
int main()
{
  foo(256-1, 0);
  foo(256-2, 0);
  foo(256-3, 0);
  foo(256-4, 0);
  bar();
  foobar();
  return 0;
}
