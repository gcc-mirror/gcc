/* { dg-do compile } */
int i;

int f (void)
{
  i >>= 2;
}

/* { dg-final { scan-assembler "asr_s" } } */
