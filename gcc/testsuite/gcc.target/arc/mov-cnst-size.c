/* Tests to check if mov instructions are emitted efficiently.  */
/* { dg-require-effective-target codedensity } */
/* { dg-options "-Os" } */

int rule1 (void)
{
  return 0x3f000000;
}

int rule2 (void)
{
  return 0x3f00;
}

int rule3 (void)
{
  return 0x3f0000;
}

int rule4 (void)
{
  return 0x22000;
}

int rule5 (void)
{
  return 0x8000001f;
}

int rule6 (void)
{
  return 0x3fffff;
}

/* { dg-final { scan-assembler "ror8\\s+r0,63" } } */
/* { dg-final { scan-assembler "lsl8\\s+r0,63" } } */
/* { dg-final { scan-assembler "lsl16\\s+r0,63" } } */
/* { dg-final { scan-assembler "ror\\s+r0,63" } } */
/* { dg-final { scan-assembler "mov_s\\s+r0,17" } } */
/* { dg-final { scan-assembler "asl_s\\s+r0,r0,13" } } */
/* { dg-final { scan-assembler "mov_s\\s+r0,-1" } } */
/* { dg-final { scan-assembler "bmsk_s\\s+r0,r0,21" } } */
