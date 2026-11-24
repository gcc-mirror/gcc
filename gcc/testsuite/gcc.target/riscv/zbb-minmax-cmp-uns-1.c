/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbb -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* Two unsigned comparisons against the same value, joined by | or &, are one
   comparison of the umin or umax of the operands:

     (A <=u C) | (B <=u C)  ==  umin (A, B) <=u C
     (A <=u C) & (B <=u C)  ==  umax (A, B) <=u C
     (A >u C) | (B >u C)    ==  umax (A, B) >u C
     (A >u C) & (B >u C)    ==  umin (A, B) >u C

   and likewise for LTU and GEU.  */

typedef unsigned long T;

int or_leu (T a, T b, T c) { return (a <= c) | (b <= c); }
int or_ltu (T a, T b, T c) { return (a < c) | (b < c); }
int or_gtu (T a, T b, T c) { return (a > c) | (b > c); }
int or_geu (T a, T b, T c) { return (a >= c) | (b >= c); }
int and_leu (T a, T b, T c) { return (a <= c) & (b <= c); }
int and_ltu (T a, T b, T c) { return (a < c) & (b < c); }
int and_gtu (T a, T b, T c) { return (a > c) & (b > c); }
int and_geu (T a, T b, T c) { return (a >= c) & (b >= c); }

/* { dg-final { scan-assembler-times "\tminu\t" 4 } } */
/* { dg-final { scan-assembler-times "\tmaxu\t" 4 } } */
/* { dg-final { scan-assembler-not "\tor\t" } } */
/* { dg-final { scan-assembler-not "\tand\t" } } */
