/* { dg-do compile } */
/* { dg-additional-options "-O3 -fdump-tree-forwprop1-details" } */

typedef int vec __attribute__((vector_size (4 * sizeof (int))));

void f (vec *p_v_in_1, vec *p_v_in_2, vec *p_v_out_1, vec *p_v_out_2)
{
  vec sel0 = { 0, 2, 0, 2 };
  vec sel1 = { 1, 3, 1, 3 };
  vec sel = { 0, 1, 6, 7 };
  vec v_1, v_2, v_x, v_y, v_out_1, v_out_2;
  vec v_in_1 = *p_v_in_1;
  vec v_in_2 = *p_v_in_2;

  /* First vec perm sequence.  */
  v_1 = __builtin_shuffle (v_in_1, v_in_1, sel0);
  v_2 = __builtin_shuffle (v_in_1, v_in_1, sel1);
  v_x = v_1 + v_2;
  v_y = v_1 - v_2;
  v_out_1 = __builtin_shuffle (v_x, v_y, sel);

  /* Second vec perm sequence.  */
  v_1 = __builtin_shuffle (v_in_2, v_in_2, sel0);
  v_2 = __builtin_shuffle (v_in_2, v_in_2, sel1);
  v_x = v_1 + v_2;
  v_y = v_1 - v_2;
  v_out_2 = __builtin_shuffle (v_x, v_y, sel);

  *p_v_out_1 = v_out_1;
  *p_v_out_2 = v_out_2;
}

/* { dg-final { scan-tree-dump "Vec perm simplify sequences have been blended" "forwprop1" } } */
/* { dg-final { scan-tree-dump "VEC_PERM_EXPR.*{ 2, 3, 6, 7 }" "forwprop1" } } */
