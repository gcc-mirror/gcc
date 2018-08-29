/* PR target/84431 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#ifdef __SIZEOF_INT128__
typedef unsigned __int128 U;
typedef signed __int128 S;
# define M 63
#else
typedef unsigned long long U;
typedef signed long long S;
# define M 31
#endif

S f1 (S a, int s) { return a >> (s & M); }
U f2 (U a, int s) { return a >> (s & M); }
U f3 (U a, int s) { return a << (s & M); }

/* { dg-final { scan-assembler-not "and" } } */
