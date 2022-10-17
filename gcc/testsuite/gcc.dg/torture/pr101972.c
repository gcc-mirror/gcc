/* { dg-do run  } */
/* { dg-require-effective-target int32plus } */

int a, b, c, d, f;
static short e = 63891;
char g = 30;
unsigned h(int i, int j) { return i << j; }
int *l(int *);
void m()
{
  a = 0;
  for (; a >= 0; a--)
    {
      int *k = &b;
      *k = e < 0;
    }
  c = b;
  l(&c);
}
int *l(int *i)
{
  d = 2;
  for (; d <= 6; d++)
    {
      if (h(d, *i) <= d)
	;
      else
	continue;
      g = 0;
      return &f;
    }
  return (void *)0;
}
int main()
{
  m();
  if (g != 30)
    __builtin_abort ();
}
