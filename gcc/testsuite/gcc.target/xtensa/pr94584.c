/* { dg-do compile } */
/* { dg-options "-O2 -mserialize-volatile" } */

unsigned long load32 (volatile unsigned long *s)
{
  return *s;
}

short load16s (volatile short *s)
{
  return *s;
}

unsigned short load16u (volatile unsigned short *s)
{
  return *s;
}

unsigned char load8 (volatile unsigned char *s)
{
  return *s;
}

/* { dg-final { scan-assembler-times "memw" 4 } } */
