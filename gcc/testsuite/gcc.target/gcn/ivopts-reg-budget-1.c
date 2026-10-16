/* Non-kernel functions may only use the first MAX_NORMAL_SGPR_COUNT SGPRs
   (see gcn_conditional_register_usage), so the register budget seen by the
   loop optimizers must be smaller for them than for kernels.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

/* A normal function.  It is expanded before main, i.e. it is the first
   function for which RTL is initialized.  */

__attribute__((noinline)) void
f (double *a, double *b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i] * 2.0;
}

double *x, *y;
int n;

/* main is compiled as a kernel.  */

int
main ()
{
  f (x, y, n);
  for (int i = 0; i < n; i++)
    x[i] += y[i];
  return 0;
}

/* { dg-final { scan-assembler "\\.amdhsa_kernel\\s+main" } } */
/* { dg-final { scan-assembler-not "\\.amdhsa_kernel\\s+f\\M" } } */
/* { dg-final { scan-tree-dump-times "target_avail_regs 44" 1 "ivopts" } } */
/* { dg-final { scan-tree-dump-times "target_avail_regs 78" 1 "ivopts" } } */
