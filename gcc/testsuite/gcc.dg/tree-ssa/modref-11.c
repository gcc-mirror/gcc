/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-modref1 -fno-ipa-pure-const"  } */
struct linkedlist {
  struct linkedlist *next;
};
struct linkedlist *
find_last (struct linkedlist *l)
{
  while (l->next)
   l = l->next;
  return l;
}
/* { dg-final { scan-tree-dump "parm 0 flags: no_direct_clobber no_indirect_clobber no_direct_escape no_indirect_escape" "modref1"} } */
