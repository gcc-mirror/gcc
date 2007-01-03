/* This used to ICE because gimplify_modify_expr_complex_part was not
   updated for the GIMPLE_MODIFY_EXPR changes in that calling
   tree_to_gimple_tuple was needed.  */

void f(void)
{
  double _Complex Res;
  __real__ Res = __imag__ Res = 0.0;
}
