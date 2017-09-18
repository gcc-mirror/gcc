/* PR tree-optimization/81346 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "\\(signed int\\) x" 10 "optimized" } } */
/* { dg-final { scan-tree-dump-times " <= 0;" 5 "optimized" } } */
/* { dg-final { scan-tree-dump-times " > 0;" 5 "optimized" } } */

int f1 (unsigned x) { return x - 1 <= __INT_MAX__ - 1; }
int f2 (unsigned x) { unsigned a = 1, b = __INT_MAX__ - 1; return x - a <= b; }
int f3 (unsigned x) { return x - 1 < __INT_MAX__; }
int f4 (unsigned x) { unsigned a = 1, b = __INT_MAX__; return x - a < b; }
int f5 (unsigned x) { return x >= 1 && x <= __INT_MAX__; }
int f6 (unsigned x) { return x - 1 > __INT_MAX__ - 1; }
int f7 (unsigned x) { unsigned a = 1, b = __INT_MAX__ - 1; return x - a > b; }
int f8 (unsigned x) { return x - 1 >= __INT_MAX__; }
int f9 (unsigned x) { unsigned a = 1, b = __INT_MAX__; return x - a >= b; }
int f10 (unsigned x) { return x < 1 || x > __INT_MAX__; }
