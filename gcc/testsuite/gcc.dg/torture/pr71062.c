/* { dg-do run } */

extern void abort (void);

char bar;

int __attribute__((noinline,noclone))
foo (char *__restrict p)
{
  if (p == &bar)
    return 1;
  return 0;
}

int main()
{
  if (foo (&bar) != 1)
    abort ();
  return 0;
}
