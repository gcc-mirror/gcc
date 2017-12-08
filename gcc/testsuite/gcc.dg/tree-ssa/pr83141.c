/* { dg-do run } */
/* { dg-options "-O -fdump-tree-esra-details" } */

volatile short vs;
volatile long vl;

struct A { short s; long i; long j; };
struct A a, b;
void foo ()
{
  struct A c;
  __builtin_memcpy (&c, &b, sizeof (struct A));
  __builtin_memcpy (&a, &c, sizeof (struct A));

  vs = c.s;
  vl = c.i;
  vl = c.j;
}


int main()
{
  if ((sizeof (short) != 2)
      || (__builtin_offsetof (struct A, i) < 4))
    return 0;

  __builtin_memset (&b, 0, sizeof (struct A));
  b.s = 1;
  __builtin_memcpy ((char *)&b+2, &b, 2);
  foo ();
  __builtin_memcpy (&a, (char *)&a+2, 2);
  if (a.s != 1)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-not "Will attempt to totally scalarize" "esra" } } */
