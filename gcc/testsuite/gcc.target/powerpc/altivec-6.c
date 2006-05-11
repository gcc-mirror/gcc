/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O0 -Wall" } */

#include <altivec.h>

/* These denote "generic" GCC vectors.  */
static int __attribute__((vector_size(16))) x, y;

static vector signed int i,j;
static vector signed short s,t;
static vector signed char c,d;
static vector float f,g;

static vector unsigned char uc;

static vector signed int *pi;

static int int1, int2;

void
b()
{
  vec_add (x, y);

  /* Make sure the predicates accept correct argument types.  */

  int1 = vec_all_in (f, g);
  int1 = vec_all_ge (f, g);
  int1 = vec_all_eq (c, d);
  int1 = vec_all_ne (s, t);
  int1 = vec_any_eq (i, j);
  int1 = vec_any_ge (f, g);
  int1 = vec_all_ngt (f, g);
  int1 = vec_any_ge (c, d);
  int1 = vec_any_ge (s, t);
  int1 = vec_any_ge (i, j);
  int1 = vec_any_ge (c, d);
  int1 = vec_any_ge (s, t);
  int1 = vec_any_ge (i, j);

  vec_mtvscr (i);
  vec_dssall ();
  s = (vector signed short) vec_mfvscr ();
  vec_dss (3);

  vec_dst (pi, int1 + int2, 3);
  vec_dstst (pi, int1 + int2, 3);
  vec_dststt (pi, int1 + int2, 3);
  vec_dstt (pi, int1 + int2, 3);

  uc = (vector unsigned char) vec_lvsl (int1 + 69, (signed int *) pi);
  uc = (vector unsigned char) vec_lvsr (int1 + 69, (signed int *) pi);

  c = vec_lde (int1, (signed char *) pi);
  s = vec_lde (int1, (signed short *) pi);
  i = vec_lde (int1, (signed int *) pi);
  i = vec_ldl (int1, pi);
  i = vec_ld (int1, pi);

  vec_st (i, int2, pi);
  vec_ste (c, int2, (signed char *) pi);
  vec_ste (s, int2, (signed short *) pi);
  vec_ste (i, int2, (signed int *) pi);
  vec_stl (i, int2, pi);
}
