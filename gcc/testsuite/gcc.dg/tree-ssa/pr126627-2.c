/* PR tree-optimization/126627 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-require-effective-target int32 } */

int test_ss (short a) { return (short)(a & -a) < 0; }
int test_ss2 (short a) { return (short)(a & -a) >= 0; }
int test_us (unsigned short a) { return (short)(a & -a) < 0; }
int test_us2 (unsigned short a) { return (short)(a & -a) >= 0; }

/* { dg-final { scan-tree-dump-not " & " "optimized" } } */
/* { dg-final { scan-tree-dump-times " == -32768" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " != -32768" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " == 32768" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " != 32768" 1 "optimized" } } */
