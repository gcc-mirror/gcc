/* { dg-additional-options "-fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

#pragma GCC optimize "-fcx-limited-range"

void do_div (_Complex double *a, _Complex double *b)
{
  *a = *b / (4.0 - 5.0fi);
}

/* { dg-final { scan-tree-dump-not "__divdc3" "optimized" { target { large_double } } } } */
/* { dg-final { scan-tree-dump-not "__divsc3" "optimized" { target { ! { large_double } } } } } */

