/* PR ipa/123227 */
/* { dg-do run } */
/* { dg-options "-Os -fipa-icf -fdump-ipa-icf-details" } */

/* The two functions have identical bodies but only one promises a non-null
   argument, so ICF must not unify them: the surviving body is allowed to
   drop the null test that the other one needs.  */

typedef struct Link Link;
struct Link
{
  int val;
  Link *next;
};

int get_vals_nonnull (Link *) __attribute__((nonnull (1)));

int
get_vals_nonnull (Link *l)
{
  int v = 0;
  for (; l; l = l->next)
    v |= l->val;
  return v;
}

int
get_vals (Link *l)
{
  int v = 0;
  for (; l; l = l->next)
    v |= l->val;
  return v;
}

int
main (void)
{
  if (get_vals ((Link *) 0) != 0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump-not "Unified" "icf" } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf" } } */
