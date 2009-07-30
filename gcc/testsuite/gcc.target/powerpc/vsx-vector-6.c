/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

void foo (vector double *out, vector double *in, vector long *p_l, vector bool long *p_b, vector unsigned char *p_uc, int *i)
{
  vector double in0 = in[0];
  vector double in1 = in[1];
  vector double in2 = in[2];
  vector long inl = *p_l;
  vector bool long inb = *p_b;
  vector unsigned char uc = *p_uc;

  *out++ = vec_abs (in0);
  *out++ = vec_add (in0, in1);
  *out++ = vec_and (in0, in1);
  *out++ = vec_and (in0, inb);
  *out++ = vec_and (inb, in0);
  *out++ = vec_andc (in0, in1);
  *out++ = vec_andc (in0, inb);
  *out++ = vec_andc (inb, in0);
  *out++ = vec_ceil (in0);
  *p_b++ = vec_cmpeq (in0, in1);
  *p_b++ = vec_cmpgt (in0, in1);
  *p_b++ = vec_cmpge (in0, in1);
  *p_b++ = vec_cmplt (in0, in1);
  *p_b++ = vec_cmple (in0, in1);
  *out++ = vec_div (in0, in1);
  *out++ = vec_floor (in0);
  *out++ = vec_madd (in0, in1, in2);
  *out++ = vec_msub (in0, in1, in2);
  *out++ = vec_max (in0, in1);
  *out++ = vec_min (in0, in1);
  *out++ = vec_msub (in0, in1, in2);
  *out++ = vec_mul (in0, in1);
  *out++ = vec_nearbyint (in0);
  *out++ = vec_nmadd (in0, in1, in2);
  *out++ = vec_nmsub (in0, in1, in2);
  *out++ = vec_nor (in0, in1);
  *out++ = vec_or (in0, in1);
  *out++ = vec_or (in0, inb);
  *out++ = vec_or (inb, in0);
  *out++ = vec_perm (in0, in1, uc);
  *out++ = vec_rint (in0);
  *out++ = vec_sel (in0, in1, inl);
  *out++ = vec_sel (in0, in1, inb);
  *out++ = vec_sub (in0, in1);
  *out++ = vec_sqrt (in0);
  *out++ = vec_trunc (in0);
  *out++ = vec_xor (in0, in1);
  *out++ = vec_xor (in0, inb);
  *out++ = vec_xor (inb, in0);

  *i++ = vec_all_eq (in0, in1);
  *i++ = vec_all_ge (in0, in1);
  *i++ = vec_all_gt (in0, in1);
  *i++ = vec_all_le (in0, in1);
  *i++ = vec_all_lt (in0, in1);
  *i++ = vec_all_nan (in0);
  *i++ = vec_all_ne (in0, in1);
  *i++ = vec_all_nge (in0, in1);
  *i++ = vec_all_ngt (in0, in1);
  *i++ = vec_all_nle (in0, in1);
  *i++ = vec_all_nlt (in0, in1);
  *i++ = vec_all_numeric (in0);
  *i++ = vec_any_eq (in0, in1);
  *i++ = vec_any_ge (in0, in1);
  *i++ = vec_any_gt (in0, in1);
  *i++ = vec_any_le (in0, in1);
  *i++ = vec_any_lt (in0, in1);
  *i++ = vec_any_nan (in0);
  *i++ = vec_any_ne (in0, in1);
  *i++ = vec_any_nge (in0, in1);
  *i++ = vec_any_ngt (in0, in1);
  *i++ = vec_any_nle (in0, in1);
  *i++ = vec_any_nlt (in0, in1);
  *i++ = vec_any_numeric (in0);
}
