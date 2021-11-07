/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-modref1"  } */
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
/* { dg-final { scan-tree-dump "noclobber noescape nodirectescape" "modref1"} } */
