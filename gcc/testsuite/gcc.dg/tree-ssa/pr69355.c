/* { dg-do run } */
/* { dg-options "-O -fno-strict-aliasing" } */

struct S
{
  void *a;
  long double b;
};

struct Z
{
  long long l;
  short s;
} __attribute__((packed));

struct S __attribute__((noclone, noinline))
foo (void *v, struct Z *z)
{
  struct S t;
  t.a = v;
  *(struct Z *) &t.b = *z;
  return t;
}

struct Z gz;

int
main (int argc, char **argv)
{
  struct S s;

  if (sizeof (long double) < sizeof (struct Z))
    return 0;

  gz.l = 0xbeef;
  gz.s = 0xab;

  s = foo ((void *) 0, &gz);

  if ((((struct Z *) &s.b)->l != gz.l)
      || (((struct Z *) &s.b)->s != gz.s))
    __builtin_abort ();
  return 0;
}
