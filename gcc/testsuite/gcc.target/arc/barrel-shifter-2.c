/* { dg-do compile } */
/* { dg-skip-if "" { ! { barrelshifter } } } */
int i;

int f (void)
{
  i >>= 2;
}

/* { dg-final { scan-assembler "asr_s" } } */
