/* PR rtl-optimization/125683 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-pre -fno-code-hoisting -fdisable-tree-phiopt1 -fdisable-tree-phiopt2 -fdisable-tree-phiopt3 -fdisable-tree-phiopt4 -fdisable-tree-cselim" } */

/* if-conversion (ce1) used to collapse the two conditional loads below
   into a single load through simplify_gen_ternary's
   (if_then_else c X X) -> X rule, which ignores the memory attributes.
   The two loads have incompatible alias sets, so the collapsed load kept
   just one of them; a later pass then treated it as not aliasing the
   long store to *d and moved it, giving the wrong value when cc == d.
   Several tree passes (PRE/code-hoisting on the release branches, phi-opt
   load factoring on trunk) can factor the two loads with a conservative
   type before RTL and so hide the bug; they are disabled here so the
   if-conversion path is exercised on every affected version.  */

long __attribute__ ((noipa))
f (int a, void *cc, long *d)
{
  long long c;
  *d = 0;
  if (a)
    c = *(long *) cc;
  else
    c = *(long long *) cc;
  *d = 1;
  return c;
}

int
main (void)
{
  long storage = -1;
  /* cc == d, and a != 0 so the taken load reads *(long *)cc, which is a
     type-compatible access to the object *d that was just set to 0.  */
  if (f (1, &storage, &storage) != 0)
    __builtin_abort ();
  return 0;
}
