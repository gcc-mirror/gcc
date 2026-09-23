/* With -ftree-coalesce-vars, one variable can have SSA names coalesced into
   partitions of other variables: the PHIs below put the two names of b into
   the partitions of a and of c.  All three variables are oversized vectors
   with no register mode, so both partitions are spilled and b legitimately
   lives in two distinct stack slots (its DECL_RTL becomes the "multiple
   places" marker).  Out-of-SSA must keep the two slots distinguishable
   without rejecting this state.  */

typedef long __attribute__((vector_size (16 * sizeof (long)))) v16di;

v16di g0 = { 1 }, g1 = { 5 }, g2 = { 3 }, g3 = { 7 };
volatile int p, q;

int
main (void)
{
  v16di b = g0;
  v16di a;
  if (p)
    a = b;
  else
    a = g1;
  g1 = a;
  b = g2;
  v16di c;
  if (q)
    c = b;
  else
    c = g3;
  g3 = c;
  if (g1[0] != 5 || g1[1] != 0 || g3[0] != 7 || g3[1] != 0)
    __builtin_abort ();
  return 0;
}
