/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O0 -Wall" } */

#define vector __attribute__((vector_size(16)))

static int vector x, y, z;

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
  z = __builtin_altivec_vadduwm (x, y);

  /* Make sure the predicates accept correct argument types.  */
  
  int1 = __builtin_altivec_vcmpbfp_p (0, f, g);
  int1 = __builtin_altivec_vcmpeqfp_p (0, f, g);
  int1 = __builtin_altivec_vcmpequb_p (0, c, d);
  int1 = __builtin_altivec_vcmpequh_p (0, s, t);
  int1 = __builtin_altivec_vcmpequw_p (0, i, j);
  int1 = __builtin_altivec_vcmpgefp_p (0, f, g);
  int1 = __builtin_altivec_vcmpgtfp_p (0, f, g);
  int1 = __builtin_altivec_vcmpgtsb_p (0, c, d);
  int1 = __builtin_altivec_vcmpgtsh_p (0, s, t);
  int1 = __builtin_altivec_vcmpgtsw_p (0, i, j);
  int1 = __builtin_altivec_vcmpgtub_p (0, c, d);
  int1 = __builtin_altivec_vcmpgtuh_p (0, s, t);
  int1 = __builtin_altivec_vcmpgtuw_p (0, i, j);

  __builtin_altivec_mtvscr (i);
  __builtin_altivec_dssall ();
  s = __builtin_altivec_mfvscr ();
  __builtin_altivec_dss (3);

  __builtin_altivec_dst (pi, int1 + int2, 3);
  __builtin_altivec_dstst (pi, int1 + int2, 3);
  __builtin_altivec_dststt (pi, int1 + int2, 3);
  __builtin_altivec_dstt (pi, int1 + int2, 3);

  uc = (vector unsigned char) __builtin_altivec_lvsl (int1 + 69, pi);
  uc = (vector unsigned char) __builtin_altivec_lvsr (int1 + 69, pi);

  c = __builtin_altivec_lvebx (int1, pi);
  s = __builtin_altivec_lvehx (int1, pi);
  i = __builtin_altivec_lvewx (int1, pi);
  i = __builtin_altivec_lvxl (int1, pi);
  i = __builtin_altivec_lvx (int1, pi);

  __builtin_altivec_stvx (i, int2, pi);
  __builtin_altivec_stvebx (c, int2, pi);
  __builtin_altivec_stvehx (s, int2, pi);
  __builtin_altivec_stvewx (i, int2, pi);
  __builtin_altivec_stvxl (i, int2, pi);
}
