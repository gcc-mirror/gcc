/* Verify that overloaded built-ins for vec_or, vec_xor, vec_nor with
 * long long inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

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

// Codegen on power7 is such that the vec_or() tests generate more xxlor
// instructions than what is seen on power8 or newer.
// Thus, an additional target close for the xxlor instruction check.
/* { dg-final { scan-assembler-times {\mxxlor\M} 6 { target p8vector_hw }  } } */
/* { dg-final { scan-assembler-times {\mxxlor\M} 24 { target { ! p8vector_hw }  }  } } */

/* { dg-final { scan-assembler-times {\mxxlxor\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlnor\M} 6 } } */
