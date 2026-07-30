/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;

/* The wrapped difference is larger than the minuend exactly when the
   subtraction borrowed, so each of these is a saturating subtract.  */

#define DEF(N, T)						\
  T f1_##N (T a, T b) { T r = a - b; return r > a ? 0 : r; }	\
  T f2_##N (T a, T b) { T r = a - b; if (r > a) r = 0; return r; } \
  T f3_##N (T a, T b) { T r = a - b; return r <= a ? r : 0; }

DEF (8, u8)
DEF (16, u16)
DEF (32, u32)
DEF (64, u64)

/* { dg-final { scan-tree-dump-times "\\.SAT_SUB " 12 "optimized" } } */
