/* Check that no unnecessary zero extensions are done on values that are
   results of arithmetic with T bit inputs.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } } */
/* { dg-final { scan-assembler-not "extu|exts" } } */

int
test00 (int a, int b, int c, int d)
{
  int x = a == b;
  int y = c == 0;
  return x == y;
}

int
test01 (int a, int b, int c, int d)
{
  int x = a == b;
  int y = c == d;
  return x == y;
}

int
test02 (int a, int b, int c, int d)
{
  int x = a != b;
  int y = c == d;
  return x == y;
}

int
test03 (int a, int b, int c, int d)
{
  int x = a != b;
  int y = c != d;
  return x == y;
}

int
test04 (int a, int b, int c, int d)
{
  int x = a != b;
  int y = c != d;
  return x == y;
}

int
test05 (int a, int b, int c, int d)
{
  int x = a == b;
  int y = c == 0;
  return x != y;
}

int
test06 (int a, int b, int c, int d)
{
  int x = a == b;
  int y = c == 0;
  return x ^ y;
}

int
test07 (int a, int b, int c, int d)
{
  int x = a == b;
  int y = c == 0;
  return x | y;
}

int
test08 (int a, int b, int c, int d)
{
  int x = a == b;
  int y = c == 0;
  return x & y;
}

int
test09 (int a, int b, int c, int d)
{
  int x = a == b;
  int y = c == d;
  return x != y;
}

int
test10 (int a, int b, int c, int d)
{
  int x = a != b;
  int y = c == d;
  return x != y;
}

int
test11 (int a, int b, int c, int d)
{
  int x = a != b;
  int y = c != d;
  return x != y;
}

int
test12 (int a, int b, int c, int d)
{
  int x = a != b;
  int y = c != d;
  return x != y;
}

int
test13 (int a, int b, int c, int d, int e, int f)
{
  int x = a == b;
  int y = c == 0;
  int z = d == e;
  return x == y || x == z;
}

int
test14 (int a, int b, int c, int d, int e, int f)
{
  int x = a == b;
  int y = c == 0;
  int z = d == e;
  return x == y && x == z;
}

int
test15 (int a, int b, int c, int d, int e, int f)
{
  int x = a != b;
  int y = c == 0;
  int z = d == e;
  return x == y || x == z;
}

int
test16 (int a, int b, int c, int d, int e, int f)
{
  int x = a != b;
  int y = c == 0;
  int z = d == e;
  return x == y && x == z;
}

int
test17 (int a, int b, int c, int d, int e, int f)
{
  int x = a != b;
  int y = c != 0;
  int z = d == e;
  return x == y || x == z;
}

int
test18 (int a, int b, int c, int d, int e, int f)
{
  int x = a != b;
  int y = c != 0;
  int z = d == e;
  return x == y && x == z;
}

int
test19 (int a, int b, int c, int d, int e, int f)
{
  int x = a != b;
  int y = c != 0;
  int z = d == e;
  return x == y || x == z;
}

int
test20 (int a, int b, int c, int d, int e, int f)
{
  int x = a != b;
  int y = c != 0;
  int z = d != e;
  return x == y && x == z;
}

int
test21 (int a, int b, int c, int d)
{
  int x = a == b;
  int y = c == 0;
  return x + y;
}

int
test22 (int a, int b, int c, int d)
{
  int x = a != b;
  int y = c == 0;
  return x + y;
}

int
test23 (int a, int b, int c, int d)
{
  int x = a != b;
  int y = c != 0;
  return x + y;
}

int
test24 (int a, int b, int c, int d)
{
  int x = a == b;
  int y = c == 0;
  return x - y;
}

int
test25 (int a, int b, int c, int d)
{
  int x = a != b;
  int y = c == 0;
  return x - y;
}

int
test26 (int a, int b, int c, int d)
{
  int x = a != b;
  int y = c != 0;
  return x - y;
}

int
test27 (int a, int b, int c, int d)
{
  int x = a == b;
  int y = c == 0;
  return x * y;
}

int
test28 (int a, int b, int c, int d)
{
  int x = a != b;
  int y = c == 0;
  return x * y;
}

int
test29 (int a, int b, int c, int d)
{
  int x = a != b;
  int y = c != 0;
  return x * y;
}

int
test30 (int a, int b)
{
  return ((a & 0x7F) == 1)
	  | ((a & 0xFF00) == 0x0200)
	  | ((a & 0xFF0000) == 0x030000);
}

int
test31 (int a, int b)
{
  return ((a & 0x7F) == 1)
	  | ((a & 0xFF00) == 0x0200)
	  | ((a & 0xFF0000) == 0x030000)
	  | ((a & 0xFF000000) == 0x04000000);
}

int
test32 (int* a, int b, int c, volatile char* d)
{
  d[1] = a[0] != 0;
  return b;
}

int
test33 (int* a, int b, int c, volatile char* d)
{
  d[1] = a[0] == 0;
  return b;
}

char
test34 (int a, int* b)
{
  return (b[4] & b[0] & a) == a;
}

unsigned char
test35 (int a, int* b)
{
  return (b[4] & b[0] & a) == a;
}
