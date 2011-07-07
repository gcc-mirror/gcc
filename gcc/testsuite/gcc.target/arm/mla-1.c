/* { dg-do compile } */
/* { dg-skip-if "" { arm_thumb1 } { "*" } { "" } } */
/* { dg-options "-O2" } */


int
foo (int *p, int *q)
{
  int i;
  int accum = 0;

  for (i = 0 ; i < 1024; i++)
    {
      accum += ((*p--) * (*q++));
      accum += 4096;
      accum >>= 13 ;
    }

  return accum;
}

/* { dg-final { scan-assembler "mla\\t" } } */
