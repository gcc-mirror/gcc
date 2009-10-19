/* https://bugzilla.redhat.com/show_bug.cgi?id=521991#c5

   Distilled from Linux XFS source code.  foo, inlined into bar, ends
   up with debug stmts referencing the addressable variable b.
   Optimization made it non-addressable, and then completely optimized
   away, before we got a chance to rename (and discard) the occurrence
   in the debug stmt.  When we did, we crashed, attempting to rename
   an unreference variable.  */

/* { dg-do compile } */

static inline int
foo (void *x, unsigned y)
{
  unsigned z = *(unsigned long *) x % y;
  *(unsigned long *) x = *(unsigned long *) x / y;
  return z;
}

struct S
{
  unsigned t;
};

void
bar (struct S *x, int *y)
{
  int a = 0;
  unsigned long b = x->t;
  foo (&b, x->t);
  for (;; a++)
    if (b)
      *y = 1;
}
