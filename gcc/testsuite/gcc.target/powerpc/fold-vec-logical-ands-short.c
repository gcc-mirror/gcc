/* Verify that overloaded built-ins for vec_and,vec_or,vec_xor with short
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O1" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector signed short
test1_and (vector bool short x, vector signed short y)
{
  vector signed short *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector signed short
test1_andc (vector bool short x, vector signed short y)
{
  vector signed short *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector signed short
test2_and (vector signed short x, vector bool short y)
{
  vector signed short *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector signed short
test2_andc (vector signed short x, vector bool short y)
{
  vector signed short *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector signed short
test3_and (vector signed short x, vector signed short y)
{
  vector signed short *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector signed short
test3_andc (vector signed short x, vector signed short y)
{
  vector signed short *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector unsigned short
test4_and (vector bool short x, vector unsigned short y)
{
  vector unsigned short *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector unsigned short
test4_andc (vector bool short x, vector unsigned short y)
{
  vector unsigned short *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector unsigned short
test5_and (vector unsigned short x, vector bool short y)
{
  vector unsigned short *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector unsigned short
test5_andc (vector unsigned short x, vector bool short y)
{
  vector unsigned short *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector unsigned short
test6_and (vector unsigned short x, vector unsigned short y)
{
  vector unsigned short *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector unsigned short
test6_andc (vector unsigned short x, vector unsigned short y)
{
  vector unsigned short *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

/* { dg-final { scan-assembler-times {\mxxland\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlandc\M} 6 } } */
