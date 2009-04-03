/* { dg-do run } */

struct T
{
  int a, b;
} t;

__attribute__((noinline)) struct T *f (struct T *p)
{
  struct T *q = __builtin_malloc (sizeof (struct T));
  *q = *p;
  return q;
}

int main (void)
{
  struct T *p;

  t.a = 1;
  t.b = 2;
  p = f (&t);
  t.a = 3;

  if (p->a != 1)
    __builtin_abort ();

  return 0;
}

