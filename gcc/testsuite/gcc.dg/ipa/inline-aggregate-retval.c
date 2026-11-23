/* The copy of the local result of 'mul' into its return value is eliminated
   by inlining, so it does not count towards the early inliner growth and
   'mul' fits in --param early-inlining-insns.  */
/* { dg-do compile } */
/* { dg-options "-O3 --param early-inlining-insns=16 -fno-inline-functions -fno-partial-inlining -fdump-tree-einline-optimized" } */

struct V { long d[8]; };

static struct V
mul (const struct V *a, const struct V *b)
{
  struct V t = *a;
  for (int i = 0; i < 8; i++)
    t.d[i] *= b->d[i];
  return t;
}

struct V ga, gb, gc, r;

void
use (void)
{
  struct V x = mul (&ga, &gb);
  r = mul (&x, &gc);
}

/* { dg-final { scan-tree-dump-times {Inlining mul/[0-9]+ into use/[0-9]+} 2 "einline" } } */
