/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Bit 1 of the mask survives the shift, so the exclusive or stays.  */
int keep (int a, int b) { return (a ^ (b & 3)) >> 1; }

/* { dg-final { scan-tree-dump-times " \\^ " 1 "optimized" } } */
