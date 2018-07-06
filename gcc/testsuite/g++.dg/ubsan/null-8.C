// PR c++/79572
// { dg-do run }
// { dg-options "-fsanitize=null -std=c++14" }
// { dg-output "reference binding to null pointer of type 'const int'" }

__attribute__((noinline, noclone))
void
bar (int x)
{
  asm volatile ("" : : "r" (x) : "memory");
}

void
foo (const int &iref)
{
  if (&iref)
    bar (iref);
  else
    bar (1);
}

int
main ()
{
  foo (*((int*) __null));
}
