/* PR middle-end/82853 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-bmi2 -mtune=generic" } */
/* { dg-final { scan-assembler-times "mul\[lq]\t" 7 } } */
/* { dg-final { scan-assembler-not "div\[lq]\t" } } */
/* { dg-final { scan-assembler-not "lea\[lq]\t\[^\n\r]*,\[^\n\r]*,2\\)" } } */

unsigned f1 (unsigned x) { return (x % 679U) == 0; }
unsigned f2 (unsigned x) { return (x % 1738U) == 0; }
void bar (void);
void f3 (unsigned x) { if (x % 3 == 0) bar (); }
void f4 (unsigned x) { if (x % 3 == 1) bar (); }
void f5 (unsigned x) { if (x % 3 == 2) bar (); }
int f6 (int x) { return x % 3 == 0; }
int f7 (int x) { return x % 6 == 0; }
