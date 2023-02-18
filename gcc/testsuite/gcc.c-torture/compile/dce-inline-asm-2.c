/* PR tree-optimization/108684 */
/* This used to ICE as when we removed the
   __builtin_unreachable in VRP, as we
   would also remove the branch and the
   inline-asm. The inline-asm had a VDEF on it,
   which we didn't update further along and
   not have the VDEF on the return statement
   updated.  */

int f (int a)
{
  asm (" " : "=X" (a) : : "memory");
  if (a)
    return 0;
  __builtin_unreachable();
}
