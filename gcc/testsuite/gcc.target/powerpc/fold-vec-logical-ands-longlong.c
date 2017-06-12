/* Verify that overloaded built-ins for vec_and,vec_or,vec_xor with long long
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

vector signed long long
test1_and (vector bool long long x, vector signed long long y)
{
  vector signed long long *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector signed long long
test1_andc (vector bool long long x, vector signed long long y)
{
  vector signed long long *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector signed long long
test2_and (vector signed long long x, vector bool long long y)
{
  vector signed long long *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector signed long long
test2_andc (vector signed long long x, vector bool long long y)
{
  vector signed long long *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector signed long long
test3_and (vector signed long long x, vector signed long long y)
{
  vector signed long long *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector signed long long
test3_andc (vector signed long long x, vector signed long long y)
{
  vector signed long long *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector unsigned long long
test4_and (vector bool long long x, vector unsigned long long y)
{
  vector unsigned long long *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector unsigned long long
test4_andc (vector bool long long x, vector unsigned long long y)
{
  vector unsigned long long *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector unsigned long long
test5_and (vector unsigned long long x, vector bool long long y)
{
  vector unsigned long long *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector unsigned long long
test5_andc (vector unsigned long long x, vector bool long long y)
{
  vector unsigned long long *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

vector unsigned long long
test6_and (vector unsigned long long x, vector unsigned long long y)
{
  vector unsigned long long *foo;
  *foo += vec_and (x, y);
  return *foo;
}

vector unsigned long long
test6_andc (vector unsigned long long x, vector unsigned long long y)
{
  vector unsigned long long *foo;
  *foo += vec_andc (x, y);
  return *foo;
}

/* { dg-final { scan-assembler-times {\mxxland\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlandc\M} 6 } } */
