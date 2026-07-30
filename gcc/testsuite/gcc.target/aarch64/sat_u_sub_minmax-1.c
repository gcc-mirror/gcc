/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#define DEF_MIN(T)					\
  T min_##T (T a, T b) { return a - (a < b ? a : b); }	\
  T nim_##T (T a, T b) { return a - (b < a ? b : a); }

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;

DEF_MIN (u8)
DEF_MIN (u16)
DEF_MIN (u32)
DEF_MIN (u64)

/* { dg-final { scan-tree-dump-times "\\.SAT_SUB " 8 "optimized" } } */
