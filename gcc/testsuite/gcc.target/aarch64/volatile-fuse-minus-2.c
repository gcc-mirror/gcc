/* { dg-do compile } */
/* { dg-options "-Os" } */

/* At -Os the subtraction of two sequenced volatile reads of *P must keep
   both reads.  */

int
sub2 (volatile int *p)
{
  int a = *p;
  int b = *p;
  return a - b;
}

long
lsub2 (volatile long *p)
{
  long a = *p;
  long b = *p;
  return a - b;
}

/* { dg-final { scan-assembler-times {\[x0\]} 4 } } */
