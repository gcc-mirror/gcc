/* PR tree-optimization/115392 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* ge with low bit of C set: e.g. b = 2 gives (b|1) >= 3 true but b >= 3 false.  */
_Bool n11(unsigned b) { return (b | 1) >= 3; }
_Bool n12(unsigned b) { return (b ^ 1) >= 3; }

/* lt with low bit of C set: e.g. b = 2 gives (b|1) < 3 false but b < 3 true.  */
_Bool n21(unsigned b) { return (b | 1) < 3; }
_Bool n22(unsigned b) { return (b ^ 1) < 3; }

/* le with low 5 bits of C not all set.  */
_Bool n31(unsigned b) { return (b | 0x12) <= 0xE0; }
_Bool n32(unsigned b) { return (b ^ 0x12) <= 0xE0; }

/* A has no known range. */
_Bool n41(unsigned a, unsigned b) { return (b | a) <= 3; }
_Bool n42(unsigned a, unsigned b) { return (b ^ a) <= 3; }

/* { dg-final { scan-tree-dump-times " \\| " 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\^ " 4 "optimized" } } */
