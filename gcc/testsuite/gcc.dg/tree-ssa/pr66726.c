/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-gimple" } */

extern unsigned short mode_size[];

int
oof (int mode)
{
  return (64 < mode_size[mode] ? 64 : mode_size[mode]);
}

/* With simplifications transforming cond_expr int min/max_expr
   supported by match.pd patterns, we can optimize this at early
   stage of compilation, rather than relying on phiopt for that.  */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 1 "gimple" } } */

