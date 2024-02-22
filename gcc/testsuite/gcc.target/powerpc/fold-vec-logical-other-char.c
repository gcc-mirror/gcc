/* Verify that overloaded built-ins for vec_orc and vec_nand with char
 * inputs produce the right results.  These intrinsics (vec_orc,
 * vec_nand) were added as part of ISA 2.07 (P8).  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O1" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

#include <altivec.h>

vector signed char
test1_orc (vector bool char x, vector signed char y)
{
  vector signed char *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector signed char
test1_nand (vector bool char x, vector signed char y)
{
  vector signed char *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

vector signed char
test2_orc (vector signed char x, vector bool char y)
{
  vector signed char *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector signed char
test2_nand (vector signed char x, vector bool char y)
{
  vector signed char *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

vector signed char
test3_orc (vector signed char x, vector signed char y)
{
  vector signed char *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector signed char
test3_nand (vector signed char x, vector signed char y)
{
  vector signed char *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

vector unsigned char
test4_orc (vector bool char x, vector unsigned char y)
{
  vector unsigned char *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector unsigned char
test4_nand (vector bool char x, vector unsigned char y)
{
  vector unsigned char *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

vector unsigned char
test5_orc (vector unsigned char x, vector bool char y)
{
  vector unsigned char *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector unsigned char
test5_nand (vector unsigned char x, vector bool char y)
{
  vector unsigned char *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

vector unsigned char
test6_orc (vector unsigned char x, vector unsigned char y)
{
  vector unsigned char *foo;
  *foo += vec_orc (x, y);
  return *foo;
}

vector unsigned char
test6_nand (vector unsigned char x, vector unsigned char y)
{
  vector unsigned char *foo;
  *foo += vec_nand (x, y);
  return *foo;
}

/* { dg-final { scan-assembler-times {\mxxlnand\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlorc\M} 6 } } */
