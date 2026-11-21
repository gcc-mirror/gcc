/* PR tree-optimization/115392 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce1" } */

/* The range of A is known only inside the branch. The dump is taken before ifcombine,
   which would otherwise in some cases rebuild an OR from the simplified conditions. */

extern void bar (void);

/* le: A in [0,7], low 3 bits of 15 all set.  */
void f11 (unsigned b, unsigned a)
{ if (a < 8) { if ((b | a) <= 15) bar (); } }
void f12 (unsigned b, unsigned a)
{ if (a < 8) { if ((b ^ a) <= 15) bar (); } }

/* gt */
void f21 (unsigned b, unsigned a)
{ if (a <= 7) { if ((b | a) > 15) bar (); } }
void f22 (unsigned b, unsigned a)
{ if (a <= 7) { if ((b ^ a) > 15) bar (); } }

/* lt: low 3 bits of 16 all clear.  */
void f31 (unsigned b, unsigned a)
{ if (a < 8) { if ((b | a) < 16) bar (); } }
void f32 (unsigned b, unsigned a)
{ if (a < 8) { if ((b ^ a) < 16) bar (); } }

/* ge: low 3 bits of 8 all clear.  */
void f41 (unsigned b, unsigned a)
{ if (a < 8) { if ((b | a) >= 8) bar (); } }
void f42 (unsigned b, unsigned a)
{ if (a < 8) { if ((b ^ a) >= 8) bar (); } }

/* Signed, range from two compares.  */
void f51 (int b, int a)
{ if (a >= 0 && a < 8) { if ((b | a) <= 15) bar (); } }
void f52 (int b, int a)
{ if (a >= 0 && a < 8) { if ((b ^ a) <= 15) bar (); } }

/* Swapped operands.  */
void f61 (unsigned b, unsigned a)
{ if (a < 8) { if ((a | b) > 15) bar (); } }
void f62 (unsigned b, unsigned a)
{ if (a < 8) { if ((a ^ b) > 15) bar (); } }

/* Signed, negative C.  */
void f71 (int b, int a)
{ if (a >= 0 && a < 8) { if ((b | a) >= -8) bar (); } }
void f72 (int b, int a)
{ if (a >= 0 && a < 8) { if ((b ^ a) >= -8) bar (); } }

/* Same constant on both tests: m = 4, low 4 bits of 15 all set.  */
void f81 (unsigned b, unsigned a)
{ if (a <= 15) { if ((b | a) <= 15) bar (); } }
void f82 (unsigned b, unsigned a)
{ if (a <= 15) { if ((b ^ a) <= 15) bar (); } }

void f91 (unsigned b, unsigned a)
{ if (a <= 15) { if ((b | a) > 15) bar (); } }
void f92 (unsigned b, unsigned a)
{ if (a <= 15) { if ((b ^ a) > 15) bar (); } }

/* Same constant on both tests: m = 4, low 4 bits of 16 all cleared.  */
void f101 (unsigned b, unsigned a)
{ if (a <= 15) { if ((b | a) < 16) bar (); } }
void f102 (unsigned b, unsigned a)
{ if (a <= 15) { if ((b ^ a) < 16) bar (); } }

/* { dg-final { scan-tree-dump-not " \\| " "cddce1" } } */
/* { dg-final { scan-tree-dump-not " \\^ " "cddce1" } } */
