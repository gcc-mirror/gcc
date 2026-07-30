/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;

#define DEF(T)						\
  T f_##T (T a, T b) { T t = ~a; return a + (b < t ? b : t); }	\
  T g_##T (T a, T b) { T t = ~a; return (t < b ? t : b) + a; }

DEF (u8)
DEF (u16)
DEF (u32)
DEF (u64)

/* { dg-final { scan-tree-dump-times "\\.SAT_ADD " 8 "optimized" } } */
