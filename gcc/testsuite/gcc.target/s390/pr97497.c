/* { dg-do run } */
/* { dg-options "-O2 -march=z900 -mzarch -fpic" } */

char *t;

void __attribute__((noinline,noclone))
bar(int a, char* b)
{
  if (a != 1)
    __builtin_abort();
}

void __attribute__((noinline,noclone))
baz(char* a, int b)
{
  if (b != 1)
    __builtin_abort();
}

int __attribute__((noinline,noclone))
foo (int a)
{
  bar (1, t);
  if (a)
    baz (t, 1);

  bar (1, t);
}

int
main ()
{
  foo (1);

  return 0;
}
