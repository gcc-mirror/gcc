/* { dg-do run } */

struct S { int *mem; };

struct S * __attribute__((noinline,noipa))
foo ()
{
  struct S *s = __builtin_malloc (sizeof (struct S));
  s->mem = __builtin_malloc (sizeof (int));
  s->mem[0] = 1;
  return s;
}

int
main()
{
  struct S *s = foo();
  if (s->mem[0] != 1)
    __builtin_abort ();
  return 0;
}
