/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadcondmov" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadcondmov" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" "-Oz" } } */

int
not_int_int (int x, int cond)
{
  if (!cond)
    return 1025;
  return x;
}

long
not_long_int (long x, int cond)
{
  if (!cond)
    return 1025l;
  return x;
}

int
not_int_long (int x, long cond)
{
  if (!cond)
    return 1025;
  return x;
}

long
not_long_long (long x, int cond)
{
  if (!cond)
    return 1025l;
  return x;
}

/* { dg-final { scan-assembler-times "th.mveqz" 4 } } */
