/* Check that the div0s instruction is used for integer sign comparisons.
   Each test case is expected to emit at least one div0s insn.
   Problems when combining the div0s comparison result with surrounding
   logic usually show up as redundant tst insns.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "div0s" 42 } } */
/* { dg-final { scan-assembler-not "tst" } } */
/* { dg-final { scan-assembler-not "not\t" } }  */
/* { dg-final { scan-assembler-not "nott" } }  */

/* { dg-final { scan-assembler-times "negc" 10 { target { ! sh2a } } } }  */
/* { dg-final { scan-assembler-times "movrt" 10 { target { sh2a } } } }  */

typedef unsigned char mybool;

int other_func_a (int, int);
int other_func_b (int, int);

mybool
test_00 (int a, int b)
{
  return (a ^ b) >= 0;
}

mybool
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

mybool
test_05 (int a, int b)
{
  return (a ^ b) < 0;
}

int
test_06 (int a, int b)
{
  return (a ^ b) < 0 ? -20 : -40;
}

mybool
test_07 (int a, int b)
{
  return (a < 0) == (b < 0);
}

int
test_08 (int a, int b)
{
  return (a < 0) == (b < 0) ? -20 : -40;
}

mybool
test_09 (int a, int b)
{
  return (a < 0) != (b < 0);
}

int
test_10 (int a, int b)
{
  return (a < 0) != (b < 0) ? -20 : -40;
}

mybool
test_11 (int a, int b)
{
  return (a >= 0) ^ (b < 0);
}

int
test_12 (int a, int b)
{
  return (a >= 0) ^ (b < 0) ? -20 : -40;
}

mybool
test_13 (int a, int b)
{
  return !((a >= 0) ^ (b < 0));
}

int
test_14 (int a, int b)
{
  return !((a >= 0) ^ (b < 0)) ? -20 : -40;
}

mybool
test_15 (int a, int b)
{
 return (a & 0x80000000) == (b & 0x80000000);
}

int
test_16 (int a, int b)
{
  return (a & 0x80000000) == (b & 0x80000000) ? -20 : -40;
}

mybool
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

mybool
test_23 (int a, int b, int c, int d)
{
  /* Should emit 2x div0s.  */
  return ((a < 0) == (b < 0)) | ((c < 0) == (d < 0));
}

mybool
test_24 (int a, int b)
{
  return a >= 0 != b >= 0;
}

mybool
test_25 (int a, int b)
{
  return !(a < 0 != b < 0);
}

int
test_26 (int a, int b, int c, int d)
{
  return a >= 0 != b >= 0 ? c : d;
}

int
test_27 (int a, int b)
{
  return a >= 0 == b >= 0;
}

int
test_28 (int a, int b, int c, int d)
{
  return a >= 0 == b >= 0 ? c : d;
}

int
test_29 (int a, int b)
{
  return ((a >> 31) ^ (b >= 0)) & 1;
}

int
test_30 (int a, int b)
{
  return ((a >> 31) ^ (b >> 31)) & 1;
}

// -------------------------------------------------------

mybool
test_31 (int a, int b)
{
  /* 2x exts.w, div0s  */
  return ((a & 0x8000) ^ (b & 0x8000)) != 0;
}

mybool
test_32 (int a, int b)
{
  /* 2x exts.w, div0s  */
  return (a & 0x8000) != (b & 0x8000);
}

mybool
test_33 (int a, int b)
{
  /* 2x add/shll, div0s  */
  return ((a & (1<<30)) ^ (b & (1<<30))) != 0;
}

mybool
test_34 (int a, int b)
{
  /* 2x exts.b, div0s  */
  return (a & 0x80) != (b & 0x80);
}

mybool
test_35 (signed char a, signed char b)
{
  /* 2x exts.b, div0s  */
  return (a < 0) != (b < 0);
}

mybool
test_36 (short a, short b)
{
  /* 2x exts.w, div0s  */
  return (a < 0) != (b < 0);
}

int
test_37 (short a, short b)
{
  /* 2x exts.w, div0s  */
  return (a < 0) != (b < 0) ? 40 : -10;
}

mybool
test_38 (int a, int b)
{
  /* 2x shll8, div0s  */
  return ((a & (1<<23)) ^ (b & (1<<23))) != 0;
}

mybool
test_39 (int a, int b)
{
  /* 2x shll2, div0s  */
  return ((a & (1<<29)) ^ (b & (1<<29))) != 0;
}

mybool
test_40 (short a, short b)
{
  /* 2x exts.w, div0s, negc  */
  return (a < 0) == (b < 0);
}
