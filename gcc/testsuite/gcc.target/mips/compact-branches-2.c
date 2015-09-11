/* { dg-options "-mcompact-branches=never" } */
int glob;

void
foo (int a, int b)
{
  if (a < b)
    glob = 1;
}

/* { dg-final { scan-assembler-not "\tb\[^ \t\]*c" } } */
/* { dg-final { scan-assembler-not "\tj\[^ \t\]*c" } } */
