/* Check that the rotcr instruction is generated.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-times "rotcr" 11 } } */

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
