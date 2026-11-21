/* PR tree-optimization/115392 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Case 1: low m bits of C all set, le/gt.  */
_Bool f11(unsigned b) { return (b | 0x12) <= 0xFF; }
_Bool f12(unsigned b) { return (b ^ 0x12) <= 0xFF; }

_Bool f21(unsigned b) { return (b | 0x12) > 0xFF; }
_Bool f22(unsigned b) { return (b ^ 0x12) > 0xFF; }

/* Case 2: low m bits of C all clear, lt/ge.  */
_Bool f31(unsigned b) { return (b | 0x12) < 0x100; }
_Bool f32(unsigned b) { return (b ^ 0x12) < 0x100; }

_Bool f41(unsigned b) { return (b | 0x12) >= 0x100; }
_Bool f42(unsigned b) { return (b ^ 0x12) >= 0x100; }

/* Original report: a has range [0,1].  */
_Bool f51(unsigned b, _Bool a) { return (b | a) <= 3; }
_Bool f52(unsigned b, _Bool a) { return (b ^ a) <= 3; }

/* Signed type, constant cannot touch the sign bit.  */
_Bool f61(int b) { return (b | 0x12) <= 0xFF; }
_Bool f62(int b) { return (b ^ 0x12) <= 0xFF; }

/* Operand order (the pattern is commutative).  */
_Bool f71(_Bool a, unsigned b) { return (a | b) <= 3; }
_Bool f72(_Bool a, unsigned b) { return (a ^ b) <= 3; }

/* Range from a mask rather than a _Bool, with lt.  */
_Bool f81(unsigned a, unsigned b) { return (b | (a & 7)) < 16; }
_Bool f82(unsigned a, unsigned b) { return (b ^ (a & 7)) < 16; }

/* Signed, negative C.  */
_Bool f91(int b) { return (b | 7) < -8; }
_Bool f92(int b) { return (b ^ 7) < -8; }

/* { dg-final { scan-tree-dump-not " \\| " "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\^ " "optimized" } } */
