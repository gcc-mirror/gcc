/* Verify that overloaded built-ins for vec_or, vec_xor, vec_nor
 * with char inputs produce the right results. */

/* { dg-do compile } */
/* { dg-options "-mvsx -O1" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector signed char
test1_or (vector bool char x, vector signed char y)
{
  vector signed char *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector signed char
test1_xor (vector bool char x, vector signed char y)
{
  vector signed char *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector signed char
test2_or (vector signed char x, vector bool char y)
{
  vector signed char *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector signed char
test2_xor (vector signed char x, vector bool char y)
{
  vector signed char *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector signed char
test3_or (vector signed char x, vector signed char y)
{
  vector signed char *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector signed char
test3_xor (vector signed char x, vector signed char y)
{
  vector signed char *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector signed char
test3_nor (vector signed char x, vector signed char y)
{
  vector signed char *foo;
  *foo += vec_nor (x, y);
  return *foo;
}


vector unsigned char
test4_or (vector bool char x, vector unsigned char y)
{
  vector unsigned char *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector unsigned char
test4_xor (vector bool char x, vector unsigned char y)
{
  vector unsigned char *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector unsigned char
test5_or (vector unsigned char x, vector bool char y)
{
  vector unsigned char *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector unsigned char
test5_xor (vector unsigned char x, vector bool char y)
{
  vector unsigned char *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector unsigned char
test6_or (vector unsigned char x, vector unsigned char y)
{
  vector unsigned char *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector unsigned char
test6_xor (vector unsigned char x, vector unsigned char y)
{
  vector unsigned char *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector unsigned char
test6_nor (vector unsigned char x, vector unsigned char y)
{
  vector unsigned char *foo;
  *foo += vec_nor (x, y);
  return *foo;
}

/* { dg-final { scan-assembler-times {\mxxlor\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlxor\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlnor\M} 2 } } */
