/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original -fdump-tree-optimized" } */

/* Neither an inclusive nor an exclusive or can carry, so an operand whose
   set bits all lie below the shift count contributes nothing.  */

int f1 (int a, int b) { return (a ^ (b & 1)) >> 1; }
unsigned f2 (unsigned a, unsigned b) { return (a | (b & 7)) >> 3; }
long f3 (long a, int c) { return (a ^ (long) (c != 0)) >> 1; }

/* GENERIC folding must preserve evaluation of the discarded operand.  */
int side;
int f4 (int a, int b) { return (a ^ ((side++, b) & 1)) >> 1; }
unsigned f5 (unsigned a, unsigned b)
{
  return ((a * 3) ^ ((side++, b) & 1)) >> 1;
}

/* { dg-final { scan-tree-dump-not " \\^ " "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\| " "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\^ " 1 "original" } } */
/* { dg-final { scan-tree-dump-not " \\| " "original" } } */
/* { dg-final { scan-tree-dump-times "side\\+\\+" 2 "original" } } */
