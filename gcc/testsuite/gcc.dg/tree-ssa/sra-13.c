/* Test that SRA replacement can deal with assignments that have
   sub-replacements on one side and a single scalar replacement on another.  */
/* { dg-do run } */
/* { dg-options "-O1" } */

struct A
{
  int i1, i2;
};

struct B
{
  long long int l;
};

union U
{
  struct A a;
  struct B b;
};

int b, gi;
long gl;
union U gu1, gu2;

int __attribute__ ((noinline, noclone))
foo (void)
{
  union U x, y;
  int r;

  y = gu1;
  if (b)
    y.b.l = gl;

  x = y;

  if (!b)
    r = x.a.i1;
  else
    r = 0;

  gu2 = x;
  return r;
}

long long int __attribute__ ((noinline, noclone))
bar (void)
{
  union U x, y;
  int r;

  y = gu1;
  if (b)
    y.a.i1 = gi;

  x = y;

  if (!b)
    r = x.b.l;
  else
    r = 0;

  gu2 = x;
  return r;
}


int
main (void)
{
  int r;
  long long int s;

  b = 0;
  gu1.a.i1 = 123;
  gu1.a.i2 = 234;
  r = foo ();
  if (r != 123)
    __builtin_abort ();
  if (gu2.a.i1 != 123)
    __builtin_abort ();
  if (gu2.a.i2 != 234)
    __builtin_abort ();

  b = 1;
  gl = 10000001;
  gu1.b.l = 10000000;
  r = foo ();
  if (r != 0)
    __builtin_abort ();
  if (gu2.b.l != 10000001)
    __builtin_abort ();

  b = 0;
  gu1.b.l = 20000000;
  s = bar ();
  if (s != (int)20000000)
    __builtin_abort ();
  if (gu2.b.l != 20000000)
    __builtin_abort ();

  b = 1;
  gi = 456;
  gu1.a.i1 = 123;
  gu1.a.i2 = 234;
  s = bar ();
  if (s != 0)
    __builtin_abort ();
  if (gu2.a.i1 != 456)
    __builtin_abort ();

  return 0;
}
