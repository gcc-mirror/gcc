/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-release_ssa" } */

unsigned int f()
{
  static const char string[] __attribute__((aligned(sizeof(int)))) = "Private";

  unsigned int priv;
  __builtin_memcpy(&priv, &string[0], sizeof(priv));
  return priv;
}

/* We should have removed the static string and simplified the
   memcpy to a store from an integer constant.  CCP
   already performs the simplification but only after release_ssa
   the unused local static is removed.  */

/* { dg-final { scan-tree-dump-not "Private" "release_ssa" } } */
