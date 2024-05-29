/* Verify that overloaded built-ins for vec_and, vec_andc, vec_or and vec_xor
 * with int inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O1" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector signed int
test1_and (vector bool int x, vector signed int y)
{
  vector signed int *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector signed int
test1_andc (vector bool int x, vector signed int y)
{
  vector signed int *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector signed int
test2_and (vector signed int x, vector bool int y)
{
  vector signed int *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector signed int
test2_andc (vector signed int x, vector bool int y)
{
  vector signed int *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector signed int
test3_and (vector signed int x, vector signed int y)
{
  vector signed int *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector signed int
test3_andc (vector signed int x, vector signed int y)
{
  vector signed int *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector unsigned int
test4_and (vector bool int x, vector unsigned int y)
{
  vector unsigned int *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector unsigned int
test4_andc (vector bool int x, vector unsigned int y)
{
  vector unsigned int *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector unsigned int
test5_and (vector unsigned int x, vector bool int y)
{
  vector unsigned int *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector unsigned int
test5_andc (vector unsigned int x, vector bool int y)
{
  vector unsigned int *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector unsigned int
test6_and (vector unsigned int x, vector unsigned int y)
{
  vector unsigned int *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector unsigned int
test6_andc (vector unsigned int x, vector unsigned int y)
{
  vector unsigned int *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

/* { dg-final { scan-assembler-times {\mxxland\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlandc\M} 6 } } */

