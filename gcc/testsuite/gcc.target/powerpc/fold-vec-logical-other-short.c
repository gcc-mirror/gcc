/* Verify that overloaded built-ins for vec_orc and vec_nand with short
 * inputs produce the right results.  These intrinsics (vec_orc, 
 * vec_nand) were added as part of ISA 2.07 (P8).  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -O1" } */

#include <altivec.h>

vector signed short
test1_orc (vector bool short x, vector signed short y)
{
  vector signed short *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector signed short
test1_nand (vector bool short x, vector signed short y)
{
  vector signed short *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

vector signed short
test2_orc (vector signed short x, vector bool short y)
{
  vector signed short *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector signed short
test2_nand (vector signed short x, vector bool short y)
{
  vector signed short *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

vector signed short
test3_orc (vector signed short x, vector signed short y)
{
  vector signed short *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector signed short
test3_nand (vector signed short x, vector signed short y)
{
  vector signed short *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

vector unsigned short
test4_orc (vector bool short x, vector unsigned short y)
{
  vector unsigned short *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector unsigned short
test4_nand (vector bool short x, vector unsigned short y)
{
  vector unsigned short *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

vector unsigned short
test5_orc (vector unsigned short x, vector bool short y)
{
  vector unsigned short *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector unsigned short
test5_nand (vector unsigned short x, vector bool short y)
{
  vector unsigned short *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

vector unsigned short
test6_orc (vector unsigned short x, vector unsigned short y)
{
  vector unsigned short *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector unsigned short
test6_nand (vector unsigned short x, vector unsigned short y)
{
  vector unsigned short *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

/* { dg-final { scan-assembler-times {\mxxlnand\M} 3 } } */
/* { dg-final { scan-assembler-times {\mxxlorc\M} 6 } } */
