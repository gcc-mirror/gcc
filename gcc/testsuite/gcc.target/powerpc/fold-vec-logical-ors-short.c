/* Verify that overloaded built-ins for vec_or, vec_xor, vec_nor with short
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O1" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector signed short
test1_or (vector bool short x, vector signed short y)
{
  vector signed short *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector signed short
test1_xor (vector bool short x, vector signed short y)
{
  vector signed short *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector signed short
test2_or (vector signed short x, vector bool short y)
{
  vector signed short *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector signed short
test2_xor (vector signed short x, vector bool short y)
{
  vector signed short *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector signed short
test3_or (vector signed short x, vector signed short y)
{
  vector signed short *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector signed short
test3_xor (vector signed short x, vector signed short y)
{
  vector signed short *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector signed short
test3_nor (vector signed short x, vector signed short y)
{
  vector signed short *foo;
  *foo += vec_nor (x, y);
  return *foo;
}

vector unsigned short
test4_or (vector bool short x, vector unsigned short y)
{
  vector unsigned short *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector unsigned short
test4_xor (vector bool short x, vector unsigned short y)
{
  vector unsigned short *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector unsigned short
test5_or (vector unsigned short x, vector bool short y)
{
  vector unsigned short *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector unsigned short
test5_xor (vector unsigned short x, vector bool short y)
{
  vector unsigned short *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector unsigned short
test6_or (vector unsigned short x, vector unsigned short y)
{
  vector unsigned short *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector unsigned short
test6_xor (vector unsigned short x, vector unsigned short y)
{
  vector unsigned short *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector unsigned short
test6_nor (vector unsigned short x, vector unsigned short y)
{
  vector unsigned short *foo;
  *foo += vec_nor (x, y);
  return *foo;
}

/* { dg-final { scan-assembler-times {\mxxlor\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlxor\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlnor\M} 2 } } */
