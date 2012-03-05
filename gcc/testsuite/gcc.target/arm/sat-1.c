/* { dg-do compile } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-require-effective-target arm_arch_v6_ok } */
/* { dg-options "-O2 -marm" } */
/* { dg-add-options arm_arch_v6 } */


static inline int sat1 (int a, int amin, int amax)
{
  if      (a < amin) return amin;
  else if (a > amax) return amax;
  else               return a;
}

static inline int sat2 (int a, int amin, int amax)
{
  if      (a > amax) return amax;
  else if (a < amin) return amin;
  else               return a;
}

int u1 (int x)
{
  return sat1 (x, 0, 63);
}

int us1 (int x)
{
  return sat1 (x >> 5, 0, 63);
}

int s1 (int x)
{
  return sat1 (x, -64, 63);
}

int ss1 (int x)
{
  return sat1 (x >> 5, -64, 63);
}

int u2 (int x)
{
  return sat2 (x, 0, 63);
}

int us2 (int x)
{
  return sat2 (x >> 5, 0, 63);
}

int s2 (int x)
{
  return sat2 (x, -64, 63);
}

int ss2 (int x)
{
  return sat2 (x >> 5, -64, 63);
}

/* { dg-final { scan-assembler-times "usat" 4 } } */
/* { dg-final { scan-assembler-times "ssat" 4 } } */

