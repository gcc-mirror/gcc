/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;

/* The same saturating subtract with the subtrahend written on the left of
   the comparison.  GIMPLE orders comparison operands by SSA version, so
   which spelling survives does not follow the source.  */

#define DEF(N, T)						\
  T f1_##N (T a, T b) { return b < a ? a - b : 0; }		\
  T f2_##N (T a, T b) { return b <= a ? a - b : 0; }		\
  T f3_##N (T a, T b) { return (T) ((a - b) * (T) (b < a)); }	\
  T f4_##N (T a, T b) { return (T) ((a - b) * (T) (b <= a)); }	\
  T f5_##N (T a, T b) { if (b >= a) return 0; return a - b; }	\
  T f6_##N (T a, T b) { return b >= a ? 0 : a - b; }		\
  T f7_##N (T a, T b) { if (a < b) return 0; return a - b; }

DEF (8, u8)
DEF (16, u16)
DEF (32, u32)
DEF (64, u64)

/* { dg-final { scan-tree-dump-times "\\.SAT_SUB " 28 "optimized" } } */
