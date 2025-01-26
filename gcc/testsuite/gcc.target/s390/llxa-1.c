/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O1 -march=arch15" } */
/* { dg-final { scan-assembler {\tllxah\t%r[0-9]+,42\(%r[0-9]+,0\)} } } */
/* { dg-final { scan-assembler {\tllxaf\t%r[0-9]+,42\(%r[0-9]+,0\)} } } */
/* { dg-final { scan-assembler {\tllxag\t%r[0-9]+,42\(%r[0-9]+,0\)} } } */
/* { dg-final { scan-assembler {\tllxaq\t%r[0-9]+,42\(%r[0-9]+,0\)} } } */

unsigned long
llxah (long j)
{
  unsigned long i = (unsigned int)j + 42;
  return i << 1;
}

unsigned long
llxaf (long j)
{
  unsigned long i = (unsigned int)j + 42;
  return i << 2;
}

unsigned long
llxag (long j)
{
  unsigned long i = (unsigned int)j + 42;
  return i << 3;
}

unsigned long
llxaq (long j)
{
  unsigned long i = (unsigned int)j + 42;
  return i << 4;
}
