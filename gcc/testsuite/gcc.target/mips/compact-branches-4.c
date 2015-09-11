/* { dg-options "-mcompact-branches=optimal isa_rev>=6" } */
int glob;

void
foo (int a, int b, volatile int * bar)
{
  if (a < b)
    glob = *bar;
}

/* { dg-final { scan-assembler "\tb\[^ \t\]*c" } } */
