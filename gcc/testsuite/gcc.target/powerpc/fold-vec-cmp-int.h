/* Verify that overloaded built-ins for vec_cmp with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -O2" } */

#include <altivec.h>

vector bool int
test3_eq (vector signed int x, vector signed int y)
{
  return vec_cmpeq (x, y);
}

vector bool int
test6_eq (vector unsigned int x, vector unsigned int y)
{
  return vec_cmpeq (x, y);
}

vector bool int
test3_ge (vector signed int x, vector signed int y)
{
  return vec_cmpge (x, y);
}

vector bool int
test6_ge (vector unsigned int x, vector unsigned int y)
{
  return vec_cmpge (x, y);
}

vector bool int
test3_gt (vector signed int x, vector signed int y)
{
  return vec_cmpgt (x, y);
}

vector bool int
test6_gt (vector unsigned int x, vector unsigned int y)
{
  return vec_cmpgt (x, y);
}

vector bool int
test3_le (vector signed int x, vector signed int y)
{
  return vec_cmple (x, y);
}

vector bool int
test6_le (vector unsigned int x, vector unsigned int y)
{
  return vec_cmple (x, y);
}

vector bool int
test3_lt (vector signed int x, vector signed int y)
{
  return vec_cmplt (x, y);
}

vector bool int
test6_lt (vector unsigned int x, vector unsigned int y)
{
  return vec_cmplt (x, y);
}

vector bool int
test3_ne (vector signed int x, vector signed int y)
{
  return vec_cmpne (x, y);
}

vector bool int
test6_ne (vector unsigned int x, vector unsigned int y)
{
  return vec_cmpne (x, y);
}

