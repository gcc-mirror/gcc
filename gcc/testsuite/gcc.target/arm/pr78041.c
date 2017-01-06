/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-fno-inline -mthumb -O1 -mfpu=neon -w" } */

extern void abort (void);

register long long x asm ("r1");

long long f (void)
{
  return x << 5;
}

int main ()
{
  x = 0x0100000001;
  if (f () != 0x2000000020)
    abort ();
  return 0;
}
