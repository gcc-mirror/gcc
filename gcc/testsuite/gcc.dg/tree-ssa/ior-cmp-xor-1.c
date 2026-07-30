/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Two values ored with the same constant agree on the bits that constant
   forces, so only the bits outside it can differ.  The dual of the existing
   (X & C) == (Y & C) rule.  */

int f1 (unsigned char a, unsigned char b) { return (a | 32) == (b | 32); }
int f2 (unsigned a, unsigned b) { return (a | 32) != (b | 32); }
int f3 (unsigned a, unsigned b) { return (a | 0xff000000u) == (b | 0xff000000u); }

/* { dg-final { scan-tree-dump-not " \\| 32" "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\| 4278190080" "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\^ " 3 "optimized" } } */
