/* PR ipa/123227 */
/* { dg-lto-do run } */
/* { dg-lto-options {{-Os -flto -fipa-icf -fdump-ipa-icf-details}} } */

/* get_vals_nonnull and get_vals have identical bodies but only one
   promises a non-null argument, so ICF must not unify them at WPA: the
   surviving body is allowed to drop the null test the other one needs.
   Both are called so that neither is removed before ICF runs, and both
   are noinline so that the calls survive as calls.  */

typedef struct Link Link;
struct Link
{
  int val;
  Link *next;
};

int get_vals_nonnull (Link *) __attribute__((nonnull (1)));

int __attribute__((noinline))
get_vals_nonnull (Link *l)
{
  int v = 0;
  for (; l; l = l->next)
    v |= l->val;
  return v;
}

extern int get_vals (Link *);

static Link one = { 5, 0 };

/* Volatile so that the null argument is not propagated into get_vals.  */
Link *volatile nullp = 0;

int
main (void)
{
  if (get_vals_nonnull (&one) != 5)
    __builtin_abort ();
  if (get_vals (nullp) != 0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-wpa-ipa-dump-not "Unified" "icf" } } */
/* { dg-final { scan-wpa-ipa-dump "Equal symbols: 0" "icf" } } */
