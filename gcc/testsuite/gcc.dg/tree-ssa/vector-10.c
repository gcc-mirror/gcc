/* { dg-do compile } */
/* { dg-additional-options "-O3 -fdump-tree-forwprop1-details -Wno-psabi" } */

typedef int vec __attribute__((vector_size (4 * sizeof (int))));

void f1 (vec *p_v_in_1, vec *p_v_in_2, vec *p_v_out_1, vec *p_v_out_2)
{
  vec sel0 = { 0, 2, 0, 2 };
  vec sel1 = { 1, 3, 1, 3 };
  vec sel = { 0, 1, 6, 7 };
  vec v_1, v_2, v_x, v_y, v_out_1, v_out_2;
  vec v_in_1 = *p_v_in_1;
  vec v_in_2;

  /* First vec perm sequence.  */
  v_1 = __builtin_shuffle (v_in_1, v_in_1, sel0);
  v_2 = __builtin_shuffle (v_in_1, v_in_1, sel1);
  v_x = v_1 + v_2;
  v_y = v_1 - v_2;
  v_out_1 = __builtin_shuffle (v_x, v_y, sel);

  /* Won't blend because v_in_2 is defined after v_1 above.  */
  v_in_2 = *p_v_in_2;
  /* Second vec perm sequence.  */
  v_1 = __builtin_shuffle (v_in_2, v_in_2, sel0);
  v_2 = __builtin_shuffle (v_in_2, v_in_2, sel1);
  v_x = v_1 + v_2;
  v_y = v_1 - v_2;
  v_out_2 = __builtin_shuffle (v_x, v_y, sel);

  *p_v_out_1 = v_out_1;
  *p_v_out_2 = v_out_2;
}

void f2 (vec *p_v_in_1, vec *p_v_in_2, vec *p_v_out_1, vec *p_v_out_2)
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
  /* Won't blend because of this store between the sequences.  */
  *p_v_out_1 = v_out_1;

  /* Second vec perm sequence.  */
  v_1 = __builtin_shuffle (v_in_2, v_in_2, sel0);
  v_2 = __builtin_shuffle (v_in_2, v_in_2, sel1);
  v_x = v_1 + v_2;
  v_y = v_1 - v_2;
  v_out_2 = __builtin_shuffle (v_x, v_y, sel);

  *p_v_out_2 = v_out_2;
}

void f3 (vec *p_v_in_1, vec *p_v_in_2, vec *p_v_out_1, vec *p_v_out_2)
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
  /* Won't blend because v_2 is RHS1 here.  */
  v_y = v_2 - v_1;
  v_out_2 = __builtin_shuffle (v_x, v_y, sel);

  *p_v_out_1 = v_out_1;
  *p_v_out_2 = v_out_2;
}

extern vec foo (vec v);
void f4 (vec *p_v_in_1, vec *p_v_out_1, vec *p_v_out_2)
{
  vec sel0 = { 0, 2, 0, 2 };
  vec sel1 = { 1, 3, 1, 3 };
  vec sel = { 0, 1, 6, 7 };
  vec v_1, v_2, v_x, v_y, v_out_1, v_out_2;
  vec v_in_1 = *p_v_in_1;
  vec v_in_2;

  /* First vec perm sequence.  */
  v_1 = __builtin_shuffle (v_in_1, v_in_1, sel0);
  v_2 = __builtin_shuffle (v_in_1, v_in_1, sel1);
  v_x = v_1 + v_2;
  v_y = v_1 - v_2;
  v_out_1 = __builtin_shuffle (v_x, v_y, sel);

  /* Won't merge because of dependency.  */
  v_in_2 = foo (v_out_1);

  /* Second vec perm sequence.  */
  v_1 = __builtin_shuffle (v_in_2, v_in_2, sel0);
  v_2 = __builtin_shuffle (v_in_2, v_in_2, sel1);
  v_x = v_1 + v_2;
  v_y = v_1 - v_2;
  v_out_2 = __builtin_shuffle (v_x, v_y, sel);

  *p_v_out_1 = v_out_1;
  *p_v_out_2 = v_out_2;
}

/* { dg-final { scan-tree-dump-not "Vec perm simplify sequences have been merged" "forwprop1" } } */
