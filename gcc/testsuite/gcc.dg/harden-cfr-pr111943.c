/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy --param=max-jump-thread-duplication-stmts=0 -Ofast -fdump-tree-hardcfr -fdump-tree-optimized" } */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */

/* Based on gcc.c-torture/compile/20050510-1.c.  */

extern void dont_remove (void);

void bar (int k)
{
  void *label = (k) ? &&x : &&y;

  if (k >= 0)
    goto *label;

x:
  if (k <= 0)
    dont_remove ();
  /* else goto y; */

y:
  return;
}

/* Check before calling dont_remove(), in the 'else goto y' edge, and in the
   abnormal edge to y.  */
/* { dg-final { scan-tree-dump-times "hardcfr_check" 3 "hardcfr" } } */
/* { dg-final { scan-tree-dump-times "hardcfr_check" 3 "optimized" } } */
/* Check that hardcfr introduces an abnormal PHI node (this could be avoided,
   but it's not worth the effort), and that it gets optimized out.  */
/* { dg-final { scan-tree-dump-times {\(ab\) = PHI .*\(ab\)} 1 "hardcfr" } } */
/* { dg-final { scan-tree-dump-not {\(ab\) = PHI .*\(ab\)} "optimized" } } */
