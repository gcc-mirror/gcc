/* Verify that overloaded built-ins for vec_and and vec_andc
 * with char inputs produce the right results. */

/* { dg-do compile } */
/* { dg-options "-mvsx -O1" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector signed char
test1_and (vector bool char x, vector signed char y)
{
  vector signed char *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector signed char
test1_andc (vector bool char x, vector signed char y)
{
  vector signed char *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector signed char
test2_and (vector signed char x, vector bool char y)
{
  vector signed char *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector signed char
test2_andc (vector signed char x, vector bool char y)
{
  vector signed char *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector signed char
test3_and (vector signed char x, vector signed char y)
{
  vector signed char *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector signed char
test3_andc (vector signed char x, vector signed char y)
{
  vector signed char *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector unsigned char
test4_and (vector bool char x, vector unsigned char y)
{
  vector unsigned char *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector unsigned char
test4_andc (vector bool char x, vector unsigned char y)
{
  vector unsigned char *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector unsigned char
test5_and (vector unsigned char x, vector bool char y)
{
  vector unsigned char *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector unsigned char
test5_andc (vector unsigned char x, vector bool char y)
{
  vector unsigned char *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector unsigned char
test6_and (vector unsigned char x, vector unsigned char y)
{
  vector unsigned char *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector unsigned char
test6_andc (vector unsigned char x, vector unsigned char y)
{
  vector unsigned char *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

/* { dg-final { scan-assembler-times {\mxxland\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlandc\M} 6 } } */

