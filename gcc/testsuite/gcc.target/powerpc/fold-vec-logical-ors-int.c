/* Verify that overloaded built-ins for vec_and, vec_andc, vec_or and vec_xor
 * with int inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O1" } */

#include <altivec.h>

vector signed int
test1_or (vector bool int x, vector signed int y)
{
  vector signed int *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector signed int
test1_xor (vector bool int x, vector signed int y)
{
  vector signed int *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector signed int
test2_or (vector signed int x, vector bool int y)
{
  vector signed int *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector signed int
test2_xor (vector signed int x, vector bool int y)
{
  vector signed int *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector signed int
test3_or (vector signed int x, vector signed int y)
{
  vector signed int *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector signed int
test3_xor (vector signed int x, vector signed int y)
{
  vector signed int *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector signed int
test3_nor (vector signed int x, vector signed int y)
{
  vector signed int *foo;
  *foo += vec_nor (x, y);
  return *foo;
}

vector unsigned int
test4_or (vector bool int x, vector unsigned int y)
{
  vector unsigned int *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector unsigned int
test4_xor (vector bool int x, vector unsigned int y)
{
  vector unsigned int *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector unsigned int
test5_or (vector unsigned int x, vector bool int y)
{
  vector unsigned int *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector unsigned int
test5_xor (vector unsigned int x, vector bool int y)
{
  vector unsigned int *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector unsigned int
test6_or (vector unsigned int x, vector unsigned int y)
{
  vector unsigned int *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector unsigned int
test6_xor (vector unsigned int x, vector unsigned int y)
{
  vector unsigned int *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector unsigned int
test6_nor (vector unsigned int x, vector unsigned int y)
{
  vector unsigned int *foo;
  *foo += vec_nor (x, y);
  return *foo;
}

/* { dg-final { scan-assembler-times {\mxxlor\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlxor\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlnor\M} 2 } } */
