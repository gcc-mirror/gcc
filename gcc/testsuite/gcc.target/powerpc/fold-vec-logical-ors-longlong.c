/* Verify that overloaded built-ins for vec_or, vec_xor, vec_nor with
 * long long inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -O2" } */

#include <altivec.h>

vector signed long long
test1_or (vector bool long long x, vector signed long long y)
{
  vector signed long long *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector signed long long
test1_xor (vector bool long long x, vector signed long long y)
{
  vector signed long long *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector signed long long
test1_nor (vector bool long long x, vector signed long long y)
{
  vector signed long long *foo;
  *foo += vec_nor (x, y);
  return *foo;
}

vector signed long long
test2_or (vector signed long long x, vector bool long long y)
{
  vector signed long long *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector signed long long
test2_xor (vector signed long long x, vector bool long long y)
{
  vector signed long long *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector signed long long
test2_nor (vector signed long long x, vector bool long long y)
{
  vector signed long long *foo;
  *foo += vec_nor (x, y);
  return *foo;
}

vector signed long long
test3_or (vector signed long long x, vector signed long long y)
{
  vector signed long long *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector signed long long
test3_xor (vector signed long long x, vector signed long long y)
{
  vector signed long long *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector signed long long
test3_nor (vector signed long long x, vector signed long long y)
{
  vector signed long long *foo;
  *foo += vec_nor (x, y);
  return *foo;
}

vector unsigned long long
test4_or (vector bool long long x, vector unsigned long long y)
{
  vector unsigned long long *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector unsigned long long
test4_xor (vector bool long long x, vector unsigned long long y)
{
  vector unsigned long long *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector unsigned long long
test4_nor (vector bool long long x, vector unsigned long long y)
{
  vector unsigned long long *foo;
  *foo += vec_nor (x, y);
  return *foo;
}

vector unsigned long long
test5_or (vector unsigned long long x, vector bool long long y)
{
  vector unsigned long long *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector unsigned long long
test5_xor (vector unsigned long long x, vector bool long long y)
{
  vector unsigned long long *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector unsigned long long
test5_nor (vector unsigned long long x, vector bool long long y)
{
  vector unsigned long long *foo;
  *foo += vec_nor (x, y);
  return *foo;
}

vector unsigned long long
test6_or (vector unsigned long long x, vector unsigned long long y)
{
  vector unsigned long long *foo;
  *foo += vec_or (x, y);
  return *foo;
}

vector unsigned long long
test6_xor (vector unsigned long long x, vector unsigned long long y)
{
  vector unsigned long long *foo;
  *foo += vec_xor (x, y);
  return *foo;
}

vector unsigned long long
test6_nor (vector unsigned long long x, vector unsigned long long y)
{
  vector unsigned long long *foo;
  *foo += vec_nor (x, y);
  return *foo;
}

// The number of xxlor instructions generated varies between 6 and 24 for
// older systems (power6,power7), as well as for 32-bit versus 64-bit targets.
// For simplicity, this test now only targets "powerpc_p8vector_ok" environments
// where the answer is expected to be 6.

/* { dg-final { scan-assembler-times {\mxxlor\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlxor\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlnor\M} 6 } } */
