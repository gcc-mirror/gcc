/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -ffloat-store -ftree-coalesce-vars -fdump-rtl-expand" } */

double in1, in2, out1, out2;

void __GIMPLE (ssa, startwith ("expand"))
f (void)
{
  double v;

__BB(2):
  v_1 = in1;
  v_2 = in2;
  out1 = v_1;
  out2 = v_2;
  return;
}

/* The scalar floating-point SSA names overlap, and -ffloat-store assigns
   both partitions to memory.  */
/* { dg-final { scan-rtl-dump {\[[0-9]+ v\+0} "expand" } } */
/* { dg-final { scan-rtl-dump {\[[0-9]+ D\.[0-9]+\+0} "expand" } } */
