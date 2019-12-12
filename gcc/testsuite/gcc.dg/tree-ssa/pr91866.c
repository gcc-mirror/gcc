/* PR middle-end/91866 */
/* { dg-do compile { target { ilp32 || lp64 } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } * /
/* { dg-final { scan-tree-dump-times " \\+ 11;" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \[+-] \[0-9-]\[0-9]*;" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\(long long unsigned int\\) x_" 5 "optimized" } } */

unsigned long long f1 (int x) { return (x + 1) - 1ULL; }
unsigned long long f2 (int x) { return (x - 5) + 5ULL; }
unsigned long long f3 (int x) { return (x - 15) + 26ULL; }
unsigned long long f4 (int x) { return (x + 6) + 5ULL; }
unsigned long long f5 (int x) { return (x - (-1 - __INT_MAX__)) + 10ULL - __INT_MAX__; }
