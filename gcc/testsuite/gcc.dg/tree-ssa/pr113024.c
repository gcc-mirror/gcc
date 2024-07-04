/* PR tree-optimization/113024 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1" } */
/* Make sure we have just a single cast per function rather than 2 casts in some cases.  */
/* { dg-final { scan-tree-dump-times " = \\\(\[a-z \]*\\\) \[xy_\]" 16 "forwprop1" { target { ilp32 || lp64 } } } } */

unsigned int f1 (signed char x) { unsigned long long y = x; return y; }
unsigned int f2 (unsigned char x) { unsigned long long y = x; return y; }
unsigned int f3 (signed char x) { long long y = x; return y; }
unsigned int f4 (unsigned char x) { long long y = x; return y; }
int f5 (signed char x) { unsigned long long y = x; return y; }
int f6 (unsigned char x) { unsigned long long y = x; return y; }
int f7 (signed char x) { long long y = x; return y; }
int f8 (unsigned char x) { long long y = x; return y; }
unsigned int f9 (signed char x) { return (unsigned long long) x; }
unsigned int f10 (unsigned char x) { return (unsigned long long) x; }
unsigned int f11 (signed char x) { return (long long) x; }
unsigned int f12 (unsigned char x) { return (long long) x; }
int f13 (signed char x) { return (unsigned long long) x; }
int f14 (unsigned char x) { return (unsigned long long) x; }
int f15 (signed char x) { return (long long) x; }
int f16 (unsigned char x) { return (long long) x; }
