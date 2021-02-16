/* { dg-options "-mno-abicalls -fno-PIC -mcompact-branches=optimal isa_rev>=6" } */
void bar (int);

void
foo ()
{
  bar (1);
}

/* { dg-final { scan-assembler "\t(bc|balc)\t" } } */
