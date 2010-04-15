/* { dg-do run } */
/* { dg-options "-O2 -fipa-pta" } */

static void __attribute__((noinline,noclone))
foo (int *p, int *q)
{
  __builtin_memcpy (p, q, sizeof (int));
}
extern void abort (void);
int main()
{
  int i = 0, j = 1;
  foo (&i, &j);
  if (i != 1)
    abort ();
  return 0;
}
