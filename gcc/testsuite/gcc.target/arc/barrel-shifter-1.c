/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=ARC601 -mbarrel-shifter" } */
int i;

int f (void)
{
  i >>= 2;
}

/* { dg-final { scan-assembler "asr_s" } } */
