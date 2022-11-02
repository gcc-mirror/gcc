/* { dg-options "-mno-abicalls -mcompact-branches=always isa_rev<=5" } */
void bar (int);

void
foo ()
{
  bar (1);
}

/* { dg-final { scan-assembler "\t(j|jal)\t" } } */
