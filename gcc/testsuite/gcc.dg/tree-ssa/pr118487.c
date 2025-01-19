/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-forwprop1-details -Wno-psabi" } */
/* { dg-additional-options "-msse2" { target i?86-*-* x86_64-*-* } } */

typedef int vec __attribute__((vector_size(16)));
vec f1_p_v_in, f1_sel00, f1_sel11, f1_sel, f1_v_out_2;
void f1() {
  vec v_1, v_2, v_x, v_y;
  v_1 = __builtin_shuffle(f1_p_v_in, f1_sel00);
  v_2 = __builtin_shuffle(f1_p_v_in, f1_sel11);
  v_x = v_2 - v_1;
  v_y = v_1 + v_2;
  f1_v_out_2 = __builtin_shuffle(v_y, v_x, f1_sel);
}

/* Won't blend because masks (e.g. f1_sel00) are not VECTOR_CSTs.  */

/* { dg-final { scan-tree-dump-not "Vec perm simplify sequences have been merged" "forwprop1" } } */
