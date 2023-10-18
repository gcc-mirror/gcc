/* { dg-do compile } */
/* { dg-options "-O3 -mavx2 -fno-tree-vrp -Wno-aggressive-loop-optimizations -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump "Avoid compile time hog on vect_peel_nonlinear_iv_init for nonlinear induction vec_step_op_mul when iteration count is too big" "vect" } } */

int r;
int r_0;

void f1 (void)
{
  int n = 0;
  while (-- n)
    {
      r_0 += r;
      r  *= 3;
    }
}
