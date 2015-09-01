/* { dg-options "-mhard-float -mcompact-branches=always isa_rev>=6 -mno-micromips" } */
int bar;

void
foo (float a, volatile int * b)
{
  if (a < 0.1)
    bar = *b;
}

/* { dg-final { scan-assembler "\t(bc1eqz|bc1nez)\t" } } */
/* { dg-final { scan-assembler "\tnop" } } */
