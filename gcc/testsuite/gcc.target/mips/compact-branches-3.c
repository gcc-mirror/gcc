/* { dg-options "-mcompact-branches=never isa_rev>=6" } */
int glob;

void
foo (int a, int b, volatile int * bar)
{
  if (a < b)
    glob = *bar;
}

/* { dg-final { scan-assembler "\tnop" } } */
/* { dg-final { scan-assembler-not "\tb\[^ \t\]*c" } } */
/* { dg-final { scan-assembler-not "\tj\[^ \t\]*c" } } */
