/* Check that shifts do not get unnecessary extends.
   See PR66706 for a case where this failed.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Each function should compile to exactly two instructions.  */
/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 16 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+blr} 8 } } */


typedef unsigned u;
typedef signed s;

u rot(u x, u n) { return (x << n) | (x >> (32 - n)); }
u shl(u x, u n) { return x << n; }
u shr(u x, u n) { return x >> n; }
s asr(s x, u n) { return x >> n; }

u roti(u x) { return (x << 23) | (x >> 9); }
u shli(u x) { return x << 23; }
u shri(u x) { return x >> 23; }
s asri(s x) { return x >> 23; }
