/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O1 -march=arch15" } */
/* { dg-final { scan-assembler {\tlxah\t%r[0-9]+,0\(%r[0-9]+,0\)} } } */
/* { dg-final { scan-assembler {\tlxaf\t%r[0-9]+,0\(%r[0-9]+,0\)} } } */
/* { dg-final { scan-assembler {\tlxag\t%r[0-9]+,0\(%r[0-9]+,0\)} } } */
/* { dg-final { scan-assembler {\tlxaq\t%r[0-9]+,0\(%r[0-9]+,0\)} } } */

long
lxah (long j)
{
  long i = (int)j;
  return i << 1;
}

long
lxaf (long j)
{
  long i = (int)j;
  return i << 2;
}

long
lxag (long j)
{
  long i = (int)j;
  return i << 3;
}

long
lxaq (long j)
{
  long i = (int)j;
  return i << 4;
}
