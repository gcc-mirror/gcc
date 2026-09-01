/* PR tree-optimization/106495 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-ethread -fdump-tree-threadfull1-details" } */

/* Minimal distillation of pr106495-1.c, pinning the never-executed
   taken-edge veto (PR105679, PR106495): a resolvable thread whose
   taken edge is probably never executed and whose destination holds
   real code (a call to abort, not just __builtin_unreachable) must
   not be threaded, lest we isolate never-executed paths that the
   late diagnostic passes then warn about.  Without the r13-1924 veto
   the 3->4 path threads straight into the abort block.
   -fdisable-tree-ethread keeps the early threader from resolving the
   path before profile estimation makes the edge cold.  */

void abort (void);
int g;

void
f (int x)
{
  if (x < 0)
    g = 1;
  if (x < 0)
    abort ();
}

/* { dg-final { scan-tree-dump "path leads to probably never executed edge" "threadfull1" } } */
