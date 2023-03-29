/* { dg-options "-mcompact-branches=always -mno-micromips isa_rev>=6" } */
int glob;

void
foo (int a, int b)
{
  if (a < b)
    glob = 1;
}

/* { dg-final { scan-assembler "\tbgec\t\\\$\[0-9\]*,\\\$\[0-9\]*" } } */
/* { dg-final { scan-assembler "\tjrc\t\\\$31" } } */
