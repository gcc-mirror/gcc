/* { dg-do compile { target rv64 } } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d" } */

/* These all have field extractions where the field is 32 or more bits, but
   any bits beyond the SI sign bit are never used.  Knowing those upper bits
   are never used, we can simplify the shift sequences a bit.  */

int
f32 (unsigned long s)
{
  return (int) ((s << 24) >> 32) + 47;
}

int
f33 (unsigned long s)
{
  return (int) ((s << 24) >> 31) + 47;
}

int
f40 (unsigned long s)
{
  return (int) ((s << 16) >> 24) + 47;
}

int
f48 (unsigned long s)
{
  return (int) ((s << 8) >> 16) + 47;
}

int
f62 (unsigned long s)
{
  return (int) ((s << 1) >> 2) + 47;
}

int
freg (unsigned long s, int y)
{
  return (int) ((s << 24) >> 32) + y;
}

/* { dg-final { scan-assembler-times "\tsrli\t" 6 } } */
/* { dg-final { scan-assembler-not "\tslli\t" } } */
