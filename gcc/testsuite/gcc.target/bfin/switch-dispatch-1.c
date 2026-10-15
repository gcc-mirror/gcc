/* PR middle-end/126236 */
/* { dg-do compile } */
/* { dg-options "-O2 -fduplicate-switch-dispatch --param=max-goto-duplication-insns=128 --param=case-values-threshold=1 -fdump-rtl-mach" } */

/* Each copy of the dispatch jumps straight into the counted loop of case 2.
   The hardware loop pass must not redirect those shared-table edges to a
   new loop entry block.  */

void
f (volatile int *vp, const unsigned char *pc, int n)
{
  for (;;)
    switch (*pc++)
      {
      case 0: return;
      case 1: *vp = 1; break;
      case 2: do *vp = 0; while (--n); n = *pc++; break;
      default: __builtin_unreachable ();
      }
}

/* { dg-final { scan-rtl-dump "entered through a shared jump table" "mach" } } */
