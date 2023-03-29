/* PR tree-optimization/108684 */
/* This used to ICE as when we remove the store to
   `t`, we also would remove the inline-asm which
   had a VDEF on it but we didn't update the
   VUSE that was later on.  */
static int t;

int f (int *a)
{
  int t1;
  asm (" " : "=X" (t1) : : "memory");
  t = t1;
  return *a;
}

