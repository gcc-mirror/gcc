/* { dg-additional-options "-fcx-limited-range -fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

#pragma GCC optimize "-fno-cx-limited-range"

void do_div (_Complex double *a, _Complex double *b)
{
  *a = *b / (4.0 - 5.0fi);
}

/* { dg-final { scan-tree-dump "__(?:gnu_)?divdc3" "optimized" } } */
