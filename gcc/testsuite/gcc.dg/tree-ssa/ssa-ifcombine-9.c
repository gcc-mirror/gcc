/* { dg-do compile } */
/* { dg-options "-O2 -fno-trapping-math -fdump-tree-ifcombine" } */

void f ();
enum Sign { NEG=-1, ZERO, POS };

static inline enum Sign sign (double x)
{
  if (x > 0) return POS;
  if (x < 0) return NEG;
  return ZERO;
}
void g (double x)
{
  if (sign (x) == NEG) f();
}

/* The above should be optimized to x < 0 by ifcombine.
   The transformation would also be legal with -ftrapping-math.  */

/* { dg-final { scan-tree-dump "optimizing.* < " "ifcombine" } } */
/* { dg-final { cleanup-tree-dump "ifcombine" } } */
