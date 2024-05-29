/* Verify that overloaded built-ins for vec_orc and vec_nand with long long
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O1" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector signed long long
test1_orc (vector bool long long x, vector signed long long y)
{
  vector signed long long *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector signed long long
test1_nand (vector bool long long x, vector signed long long y)
{
  vector signed long long *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

vector signed long long
test2_orc (vector signed long long x, vector bool long long y)
{
  vector signed long long *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector signed long long
test2_nand (vector signed long long x, vector bool long long y)
{
  vector signed long long *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

vector signed long long
test3_orc (vector signed long long x, vector signed long long y)
{
  vector signed long long *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector signed long long
test3_nand (vector signed long long x, vector signed long long y)
{
  vector signed long long *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

vector unsigned long long
test4_orc (vector bool long long x, vector unsigned long long y)
{
  vector unsigned long long *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector unsigned long long
test4_nand (vector bool long long x, vector unsigned long long y)
{
  vector unsigned long long *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

vector unsigned long long
test5_orc (vector unsigned long long x, vector bool long long y)
{
  vector unsigned long long *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector unsigned long long
test5_nand (vector unsigned long long x, vector bool long long y)
{
  vector unsigned long long *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

vector unsigned long long
test6_orc (vector unsigned long long x, vector unsigned long long y)
{
  vector unsigned long long *foo;
  *foo += vec_orc (x, y);
  return *foo;
}
vector unsigned long long
test6_nand (vector unsigned long long x, vector unsigned long long y)
{
  vector unsigned long long *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

/* { dg-final { scan-assembler-times {\mxxlnand\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlorc\M} 6 } } */
