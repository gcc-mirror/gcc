/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=power9 -maltivec -O2" } */

/* Ensure that if we set a pragma gcc target for an
   older processor, we do not compile builtins that
   the older target does not support.  */

#include <altivec.h>

vector bool int
test1 (vector signed int a, vector signed int b)
{
  return vec_cmpnez (a, b);
}

#pragma GCC target ("cpu=power8")
vector bool int
test2 (vector signed int a, vector signed int b)
{
  return vec_cmpnez (a, b);
  /* { dg-error "'__builtin_altivec_vcmpnezw' requires the '-mcpu=power9' and '-mvsx' options" "" { target *-*-* } .-1 } */
}

#pragma GCC target ("cpu=power7")
vector signed int
test3 (vector signed int a, vector signed int b)
{
  return vec_mergee (a, b);
  /* { dg-error "'__builtin_altivec_vmrgew_v4si' requires the '-mcpu=power8' and '-mvsx' options" "" { target *-*-* } .-1 } */
}

#pragma GCC target ("cpu=power6")
vector signed int
test4 (vector int a, vector int b)
{
  return vec_sldw (a, b, 2);
  /* { dg-error "'__builtin_vsx_xxsldwi_4si' requires the '-mvsx' option" "" { target *-*-* } .-1 } */
}

vector int
test5 (vector int a, vector int b)
{
  return vec_add (a, b);
}

