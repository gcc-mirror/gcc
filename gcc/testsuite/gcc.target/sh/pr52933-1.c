/* Check that the div0s instruction is used for integer sign comparisons.
   Each test case is expected to emit at least one div0s insn.
   Problems when combining the div0s comparison result with surrounding
   logic usually show up as redundant tst insns.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } } */
/* { dg-final { scan-assembler-times "div0s" 25 } } */
/* { dg-final { scan-assembler-not "tst" } } */

typedef unsigned char bool;

int other_func_a (int, int);
int other_func_b (int, int);

bool
test_00 (int a, int b)
{
  return (a ^ b) >= 0;
}

bool
test_01 (int a, int b)
{
  return (a ^ b) < 0;
}

int
test_02 (int a, int b, int c, int d)
{
  if ((a ^ b) < 0)
    return other_func_a (a, c);
  else
    return other_func_b (d, b);
}

int
test_03 (int a, int b, int c, int d)
{
  if ((a ^ b) >= 0)
    return other_func_a (a, c);
  else
    return other_func_b (d, b);
}

int
test_04 (int a, int b)
{
  return (a ^ b) >= 0 ? -20 : -40;
}

bool
test_05 (int a, int b)
{
  return (a ^ b) < 0;
}

int
test_06 (int a, int b)
{
  return (a ^ b) < 0 ? -20 : -40;
}

bool
test_07 (int a, int b)
{
  return (a < 0) == (b < 0);
}

int
test_08 (int a, int b)
{
  return (a < 0) == (b < 0) ? -20 : -40;
}

bool
test_09 (int a, int b)
{
  return (a < 0) != (b < 0);
}

int
test_10 (int a, int b)
{
  return (a < 0) != (b < 0) ? -20 : -40;
}

bool
test_11 (int a, int b)
{
  return (a >= 0) ^ (b < 0);
}

int
test_12 (int a, int b)
{
  return (a >= 0) ^ (b < 0) ? -20 : -40;
}

bool
test_13 (int a, int b)
{
  return !((a >= 0) ^ (b < 0));
}

int
test_14 (int a, int b)
{
  return !((a >= 0) ^ (b < 0)) ? -20 : -40;
}

bool
test_15 (int a, int b)
{
 return (a & 0x80000000) == (b & 0x80000000);
}

int
test_16 (int a, int b)
{
  return (a & 0x80000000) == (b & 0x80000000) ? -20 : -40;
}

bool
test_17 (int a, int b)
{
  return (a & 0x80000000) != (b & 0x80000000);
}

int
test_18 (int a, int b)
{
  return (a & 0x80000000) != (b & 0x80000000) ? -20 : -40;
}

int
test_19 (unsigned int a, unsigned int b)
{
  return (a ^ b) >> 31;
}

int
test_20 (unsigned int a, unsigned int b)
{
  return (a >> 31) ^ (b >> 31);
}

int
test_21 (int a, int b)
{
  return ((a & 0x80000000) ^ (b & 0x80000000)) >> 31 ? -30 : -10;
}

int
test_22 (int a, int b, int c, int d)
{
  if ((a < 0) == (b < 0))
    return other_func_a (a, b);
  else
    return other_func_b (c, d);
}

bool
test_23 (int a, int b, int c, int d)
{
  /* Should emit 2x div0s.  */
  return ((a < 0) == (b < 0)) | ((c < 0) == (d < 0));
}
