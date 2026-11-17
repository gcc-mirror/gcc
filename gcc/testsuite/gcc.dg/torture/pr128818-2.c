/* { dg-do run } */

struct A { int f; };
struct B { int f; };

__attribute__((noinline))
int foo (struct B *b1, void *p, int c, int v)
{
  struct B *b = (struct B *)p;
  struct A *a = (struct A *)p;

  b1->f = 1;
  b->f = v;

  int k;
  if (c)
    k = 0;
  else
    k = c;

  a[k].f = v;
  b->f = v;

  return b1->f;
}

int main ()
{
  void *p = __builtin_malloc (sizeof (struct A) > sizeof (struct B)
			      ? sizeof (struct A) : sizeof (struct B));
  int res = foo ((struct B *)p, p, 0, 42);
  if (res != 42)
    __builtin_abort ();
  return 0;
}
