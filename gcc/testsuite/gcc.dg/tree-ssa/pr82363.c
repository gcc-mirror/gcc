/* { dg-do run } */
/* { dg-options "-O" } */

struct A
{
  int b;
  int c;
  int d;
};

struct E
{
  int f;
  int g:18;
  struct A h;
};

struct I
{
  int b;
  int j;
  struct E k;
};

int l, *m = &l;

struct A n;
struct I o;

void __attribute__ ((noipa))
test_l (void)
{
  if (l != 1)
    __builtin_abort ();
}

int main ()
{
  while (1)
    {
      struct I q = { 0, 0, {0, 0, {1, 1, 1}}}, p = q, r = p, *s = &q;
      if (p.k.h.c)
        o = p;
      *m = r.k.h.d;
      n = (*s).k.h;
      break;
    }
  test_l ();
  return 0;
}
