/* PR target/48688 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int fn1 (int x) { return (x << 3) | 5; }
int fn2 (int x) { return (x * 8) | 5; }
int fn3 (int x) { return (x << 3) + 5; }
int fn4 (int x) { return (x * 8) + 5; }
int fn5 (int x) { return (x << 3) ^ 5; }
int fn6 (int x) { return (x * 8) ^ 5; }
long fn7 (long x) { return (x << 3) | 5; }
long fn8 (long x) { return (x * 8) | 5; }
long fn9 (long x) { return (x << 3) + 5; }
long fn10 (long x) { return (x * 8) + 5; }
long fn11 (long x) { return (x << 3) ^ 5; }
long fn12 (long x) { return (x * 8) ^ 5; }
long fn13 (unsigned x) { return (x << 3) | 5; }
long fn14 (unsigned x) { return (x * 8) | 5; }
long fn15 (unsigned x) { return (x << 3) + 5; }
long fn16 (unsigned x) { return (x * 8) + 5; }
long fn17 (unsigned x) { return (x << 3) ^ 5; }
long fn18 (unsigned x) { return (x * 8) ^ 5; }

/* { dg-final { scan-assembler-not "\[ \t\]x?or\[bwlq\]\[ \t\]" } } */
