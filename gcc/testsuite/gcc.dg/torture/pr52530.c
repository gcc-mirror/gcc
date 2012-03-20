/* { dg-do run } */

extern void abort (void);

#if __SIZEOF_INT__ > 2
struct foo
{
 int *f;
 int i;
};

int baz;
#else
struct foo
{
 long *f;
 long i;
};

long baz;
#endif

void __attribute__ ((noinline))
bar (struct foo x)
{
 *(x.f) = x.i;
}

int
main ()
{
  struct foo x = { &baz, 0xdeadbeef };

  bar (x);

  if (baz != 0xdeadbeef)
    abort ();

  return 0;
}
