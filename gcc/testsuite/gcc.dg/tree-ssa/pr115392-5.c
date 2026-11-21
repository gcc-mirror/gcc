/* PR tree-optimization/115392 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce1" } */

extern void bar (void);

/* a in [0,8] needs m = 4, low 4 bits of 7 are not all set.  */
void n11 (unsigned b, unsigned a)
{ if (a <= 8) { if ((b | a) <= 7) bar (); } }
void n12 (unsigned b, unsigned a)
{ if (a <= 8) { if ((b ^ a) <= 7) bar (); } }

/* a may be negative, so the sign bit can be set.  */
void n21 (int b, int a)
{ if (a >= -1 && a <= 3) { if ((b | a) <= 3) bar (); } }
void n22 (int b, int a)
{ if (a >= -1 && a <= 3) { if ((b ^ a) <= 3) bar (); } }

/* ge with low bit of C set.  */
void n31 (unsigned b, unsigned a)
{ if (a < 8) { if ((b | a) >= 5) bar (); } }
void n32 (unsigned b, unsigned a)
{ if (a < 8) { if ((b ^ a) >= 5) bar (); } }

/* No range information at all.  */
void n41 (unsigned b, unsigned a)
{ if ((b | a) <= 15) bar (); }
void n42 (unsigned b, unsigned a)
{ if ((b ^ a) <= 15) bar (); }

/* { dg-final { scan-tree-dump-times " \\| " 4 "cddce1" } } */
/* { dg-final { scan-tree-dump-times " \\^ " 4 "cddce1" } } */
