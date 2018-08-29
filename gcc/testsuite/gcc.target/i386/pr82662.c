/* PR target/82580 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#ifdef __SIZEOF_INT128__
typedef unsigned __int128 U;
typedef signed __int128 S;
#else
typedef unsigned long long U;
typedef signed long long S;
#endif
void bar (void);
int f0 (U x, U y) { return x == y; }
int f1 (U x, U y) { return x != y; }
int f2 (U x, U y) { return x > y; }
int f3 (U x, U y) { return x >= y; }
int f4 (U x, U y) { return x < y; }
int f5 (U x, U y) { return x <= y; }
int f6 (S x, S y) { return x == y; }
int f7 (S x, S y) { return x != y; }
int f8 (S x, S y) { return x > y; }
int f9 (S x, S y) { return x >= y; }
int f10 (S x, S y) { return x < y; }
int f11 (S x, S y) { return x <= y; }

/* { dg-final { scan-assembler-times {\mset} 12 } } */
