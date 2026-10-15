/* PR middle-end/126236 */
/* { dg-do compile } */
/* { dg-additional-options "-fduplicate-switch-dispatch --param=max-goto-duplication-insns=128 --param=case-values-threshold=1" } */

/* The counted loop of case 3 starts right at its case label, and its count
   is live into the dispatch, so each copy of the dispatch jumps straight
   into the loop.  Case 4 also enters the loop by falling through.  Targets
   with hardware loops must not redirect the shared-table edges to a new
   loop entry block.  */

void
f (volatile int *vp, const unsigned char *pc, const int *ip)
{
  int n = *ip++;
  for (;;)
    switch (*pc++)
      {
      case 0: return;
      case 1: *vp = 1; break;
      case 2: n = *ip++; break;
      case 4: *vp = 4;
	/* Fall through.  */
      case 3: do *vp = 0; while (--n); n = *ip++; break;
      default: __builtin_unreachable ();
      }
}
