/* { dg-options "-mno-abicalls -mcompact-branches=never isa_rev>=6" } */
void bar (int);

void
foo ()
{
  bar (1);
}

/* { dg-final { scan-assembler "\t(j|jal)\t" } } */
