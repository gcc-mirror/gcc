/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Only the low bits of the addition survive the mask, so the constant
   moves to the other side of the comparison.  */
int f1 (unsigned int x) { return ((x + 3u) & 7u) == 5u; }
int f2 (unsigned int x) { return ((x + 300u) & 255u) != 7u; }

/* The comparison constant does not fit the mask, the result is fixed.  */
int f3 (unsigned int x) { return ((x + 3u) & 7u) == 9u; }

/* Clearing the low bits rounds down, so the comparison constant can absorb
   them and the mask goes away.  */
int f4 (int x) { return (x & -8) > 16; }
int f5 (int x) { return (x & -8) <= -16; }
int f6 (unsigned int x) { return (x & 0xfffffff0u) > 100u; }

/* { dg-final { scan-tree-dump-not " \\+ " "optimized" } } */
/* { dg-final { scan-tree-dump-times " & 7;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & 255" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " > 23" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " < -8" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " > 111" 1 "optimized" } } */
