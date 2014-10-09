/* { dg-do compile } */
/* { dg-options "-O2 -march=k8" } */
/* { dg-final { scan-assembler "sbb" } } */

extern void abort (void);

/* Conditional increment is best done using sbb $-1, val.  */
int t[]={0,0,0,0,1,1,1,1,1,1};
void
q()
{
  int sum=0;
  int i;
  for (i=0;i<10;i++)
    if (t[i])
       sum++;
  if (sum != 6)
    abort ();
}
int
main()
{
  int i;
  for (i=0;i<10000000;i++)
    q();
}
