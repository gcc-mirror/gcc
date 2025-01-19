/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-forwprop1-details -Wno-psabi" } */
/* { dg-additional-options "-msse2" { target i?86-*-* x86_64-*-* } } */

typedef int vec __attribute__((vector_size (4 * sizeof (int))));

void f1 (vec *p_v_in, vec *p_v_out_1, vec *p_v_out_2)
{
  vec sel00 = { 2, 3, 2, 2 };
  vec sel01 = { 1, 0, 1, 1 };
  vec sel10 = { 3, 2, 3, 3 };
  vec sel11 = { 0, 1, 0, 0 };
  vec sel = { 0, 5, 2, 7 };
  vec v_1, v_2, v_x, v_y, v_out_1, v_out_2;
  vec v_in = *p_v_in;

  /* First vec perm sequence.  */
  v_1 = __builtin_shuffle (v_in, v_in, sel00);
  v_2 = __builtin_shuffle (v_in, v_in, sel01);
  v_x = v_2 - v_1;
  v_y = v_1 + v_2;
  v_out_1 = __builtin_shuffle (v_y, v_x, sel);

  /* Second vec perm sequence.  */
  v_1 = __builtin_shuffle (v_in, v_in, sel10);
  v_2 = __builtin_shuffle (v_in, v_in, sel11);
  v_x = v_2 - v_1;
  v_y = v_1 + v_2;
  v_out_2 = __builtin_shuffle (v_y, v_x, sel);

  *p_v_out_1 = v_out_1;
  *p_v_out_2 = v_out_2;
}

/* { dg-final { scan-tree-dump "Vec perm simplify sequences have been blended" "forwprop1" { target { aarch64*-*-* i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-tree-dump "VEC_PERM_EXPR.*{ 2, 7, 2, 6 }" "forwprop1" { target { aarch64*-*-* i?86-*-* x86_64-*-* } } } } */
