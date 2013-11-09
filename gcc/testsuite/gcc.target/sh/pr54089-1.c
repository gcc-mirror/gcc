/* Check that the rotcr instruction is generated.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-times "rotcr" 24 } } */
/* { dg-final { scan-assembler-times "shll\t" 1 } } */

typedef char bool;

long long
test_00 (long long a)
{
  return a >> 1;
}

unsigned int
test_01 (unsigned int a, int b, int c)
{
  bool r = b == c;
  return ((a >> 1) | (r << 31));
}

unsigned int
test_02 (unsigned int a, int b, int c)
{
  bool r = b == c;
  return ((a >> 2) | (r << 31));
}

unsigned int
test_03 (unsigned int a, int b, int c)
{
  bool r = b == c;
  return ((a >> 3) | (r << 31));
}

unsigned int
test_04 (unsigned int a, int b, int c)
{
  bool r = b == c;
  return ((a >> 4) | (r << 31));
}

unsigned int
test_05 (unsigned int a, int b, int c)
{
  bool r = b == c;
  return ((a >> 5) | (r << 31));
}

unsigned int
test_06 (unsigned int a, int b, int c)
{
  bool r = b == c;
  return ((a >> 6) | (r << 31));
}

unsigned int
test_07 (unsigned int a, int b, int c)
{
  bool r = b == c;
  return ((a >> 7) | (r << 31));
}

unsigned int
test_08 (unsigned int a, int b, int c)
{
  bool r = b == c;
  return ((a >> 8) | (r << 31));
}

unsigned int
test_09 (unsigned int a, int b, int c)
{
  bool r = b == c;
  return ((a >> 31) | (r << 31));
}

int
test_10 (int a, int b)
{
  bool r = a == b;
  return r << 31;
}

unsigned int
test_11 (unsigned int a, int b)
{
  /* 1x shlr, 1x rotcr  */
  return (a >> 1) | (b << 31);
}

unsigned int
test_12 (unsigned int a, int b)
{
  return (a >> 2) | (b << 31);
}

unsigned int
test_13 (unsigned int a, int b)
{
  return (a >> 3) | (b << 31);
}

unsigned int
test_14 (unsigned int a, int b)
{
  /* 1x shll, 1x rotcr  */
  bool r = b < 0;
  return ((a >> 1) | (r << 31));
}

unsigned int
test_15 (unsigned int a, int b, int c)
{
  bool r = b != c;
  return ((a >> 1) | (r << 31));
}

unsigned int
test_16 (unsigned int a, int b, int c)
{
  bool r = b != c;
  return ((a >> 2) | (r << 31));
}

unsigned int
test_17 (unsigned int a, int b, int c)
{
  bool r = b != c;
  return ((a >> 3) | (r << 31));
}

unsigned int
test_18 (unsigned int a, int b, int c)
{
  bool r = b != c;
  return ((a >> 4) | (r << 31));
}

unsigned int
test_19 (unsigned int a, int b, int c)
{
  bool r = b != c;
  return ((a >> 5) | (r << 31));
}

unsigned int
test_20 (unsigned int a, int b, int c)
{
  bool r = b != c;
  return ((a >> 6) | (r << 31));
}

unsigned int
test_21 (unsigned int a, int b, int c)
{
  bool r = b != c;
  return ((a >> 7) | (r << 31));
}

unsigned int
test_22 (unsigned int a, int b, int c)
{
  bool r = b != c;
  return ((a >> 8) | (r << 31));
}

unsigned int
test_23 (unsigned int a, int b, int c)
{
  bool r = b != c;
  return ((a >> 31) | (r << 31));
}
