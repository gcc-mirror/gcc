/* Tests to check the utilization of the addc instruction in special cases.
   If everything works as expected we won't see any movt instructions in
   these cases.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */

/* { dg-final { scan-assembler-times "addc" 36 } } */
/* { dg-final { scan-assembler-times "shll" 14 } } */
/* { dg-final { scan-assembler-times "add\tr" 12 } } */
/* { dg-final { scan-assembler-not "movt" } } */

/* { dg-final { scan-assembler-times "add\t#1" 1 } } */

/* { dg-final { scan-assembler-times "mov\t#-2" 1 { target { ! sh2a } } } } */
/* { dg-final { scan-assembler-times "and\tr" 1 { target { ! sh2a } } } } */

/* { dg-final { scan-assembler-times "bclr\t#0" 1 { target { sh2a } } } } */

/* { dg-final { scan-assembler-times "shlr" 22 { target { ! sh2a } } } } */
/* { dg-final { scan-assembler-not "shlr" { target { sh2a } } } } */

int
test_000 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x addc
  return a + (b & 1);
}

int
test_001 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x addc
  return a + b + (c & 1);
}

int
test_002 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x add, 1x addc
  return a + b + c + (d & 1);
}

int
test_003 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x addc
  return (b & 1) + a;
}

int
test_004 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x addc
  return a + (c & 1) + b;
}

int
test_005 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x add, 1x addc
  return a + b + (d & 1) + c;
}

int
test_006 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x addc
  return (c & 1) + a + b;
}

int
test_007 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x add, 1x addc
  return a + (d & 1) + b + c;
}

int
test_008 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x add, 1x addc
  return (d & 1) + a + b + c;
}

int
test_009 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x addc
  return a + b + (b & 1);
}

int
test_010 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x addc
  return a + (b & 1) + b;
}

int
test_011 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x addc
  return (b & 1) + a + b;
}

int
test_012 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x add, 1x addc
  return a + b + d + (b & 1);
}

int
test_013 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x add, 1x addc
  return a + d + (b & 1) + b;
}

int
test_014 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x add, 1x addc
  return a + (b & 1) + d + b;
}

int
test_015 (int a, int c, int b, int d)
{
  // 1x shlr/bld, 1x add, 1x addc
  return (b & 1) + a + d + b;
}

int
test_016 (int a, int b, int c, int d)
{
  // non-SH2A: 1x add #1, 1x mov #-2, 1x and
  // SH2A: 1x add #1, 1x bclr #0
  return a + (a & 1);
}

int
test_017 (int a, int b, int c, int d)
{
  // 1x shlr/bld, 1x addc
  return a + a + (a & 1);
}

int
test_018 (int a, int b, int c, int d)
{
  // 1x shlr/bld, 1x addc
  return a + (a & 1) + a;
}

int
test_019 (int a, int b, int c, int d)
{
  // 1x shlr/bld, 1x addc
  return (a & 1) + a + a;
}

int
test_020 (int a, int b, int c, int d)
{
  // 1x shlr/bld, 1x addc
  return b + b + (a & 1);
}

int
test_021 (int a, int b, int c, int d)
{
  // 1x shlr/bld, 1x addc
  return b + (a & 1) + b;
}

int
test_022 (int a, int b, int c, int d)
{
  // 1x shlr/bld, 1x addc
  return (a & 1) + b + b;
}

int
test_023 (int a, int b, int c, int d)
{
  // 1x shll, 1x addc
  return a + ((b >> 31) & 1);
}

int
test_024 (int a, int b, int c, int d)
{
  // 1x shll, 1x addc
  return ((b >> 31) & 1) + a;
}

int
test_025 (int a, int b, int c, int d)
{
  // 1x shll, 1x addc
  return ((a >> 31) & 1) + a;
}

int
test_026 (int a, int b, int c, int d)
{
  // 1x shll, 1x addc
  return a + ((a >> 31) & 1);
}

int
test_027 (int a, int b, int c, int d)
{
  // 1x shll, 1x addc
  return a + b + ((c >> 31) & 1);
}

int
test_028 (int a, int b, int c, int d)
{
  // 1x shll, 1x addc
  return a + ((c >> 31) & 1) + b;
}

int
test_029 (int a, int b, int c, int d)
{
  // 1x shll, 1x addc
  return ((c >> 31) & 1) + a + b;
}

int
test_030 (int a, int b, int c, int d)
{
  // 1x shll, 1x addc, 1x add
  return a + b + c + ((d >> 31) & 1);
}

int
test_031 (int a, int b, int c, int d)
{
  // 1x shll, 1x addc, 1x add
  return a + b + ((d >> 31) & 1) + c;
}

int
test_032 (int a, int b, int c, int d)
{
  // 1x shll, 1x addc, 1x add
  return a + ((d >> 31) & 1) + b + c;
}

int
test_033 (int a, int b, int c, int d)
{
  // 1x shll, 1x addc, 1x add
  return ((d >> 31) & 1) + a + b + c;
}

int
test_034 (int a, int b, int c, int d)
{
  // 1x shll, 1x addc
  return a + a + ((d >> 31) & 1);
}

int
test_035 (int a, int b, int c, int d)
{
  // 1x shll, 1x addc
  return a + ((d >> 31) & 1) + a;
}

int
test_036 (int a, int b, int c, int d)
{
  // 1x shll, 1x addc
  return ((d >> 31) & 1) + a + a;
}
