/* { dg-do compile } */
/* { dg-options "-O -fdump-rtl-subreg" } */
/* { dg-require-effective-target ilp32 } */

long long test (long long a, long long b) { return a | b; }

/* { dg-final { scan-rtl-dump "Splitting reg" "subreg" } } */
/* { dg-final { cleanup-tree-dump "subreg" } } */
