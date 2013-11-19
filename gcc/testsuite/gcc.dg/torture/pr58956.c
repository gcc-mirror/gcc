/* { dg-do run } */

extern void abort (void);

struct S
{
  int f0;
} a = {1}, b, g, *c = &b, **f = &c;

int *d, **e = &d, h;

struct S
foo ()
{
  *e = &h;
  if (!d) 
    __builtin_unreachable ();
  *f = &g;
  return a;
}

int
main ()
{
  struct S *i = c;
  *i = foo ();
  if (b.f0 != 1)
    abort ();
  return 0;
}
