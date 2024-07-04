/* Verify that overloaded built-ins for vec_cmp with short
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector bool short
test3_eq (vector signed short x, vector signed short y)
{
  return vec_cmpeq (x, y);
}

vector bool short
test6_eq (vector unsigned short x, vector unsigned short y)
{
  return vec_cmpeq (x, y);
}

vector bool short
test3_ge (vector signed short x, vector signed short y)
{
  return vec_cmpge (x, y);
}

vector bool short
test6_ge (vector unsigned short x, vector unsigned short y)
{
  return vec_cmpge (x, y);
}

vector bool short
test3_gt (vector signed short x, vector signed short y)
{
  return vec_cmpgt (x, y);
}

vector bool short
test6_gt (vector unsigned short x, vector unsigned short y)
{
  return vec_cmpgt (x, y);
}


vector bool short
test3_le (vector signed short x, vector signed short y)
{
  return vec_cmple (x, y);
}

vector bool short
test6_le (vector unsigned short x, vector unsigned short y)
{
  return vec_cmple (x, y);
}

vector bool short
test3_lt (vector signed short x, vector signed short y)
{
  return vec_cmplt (x, y);
}

vector bool short
test6_lt (vector unsigned short x, vector unsigned short y)
{
  return vec_cmplt (x, y);
}

vector bool short
test3_ne (vector signed short x, vector signed short y)
{
  return vec_cmpne (x, y);
}

vector bool short
test6_ne (vector unsigned short x, vector unsigned short y)
{
  return vec_cmpne (x, y);
}

