/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;

/* Every one of these tests for overflow against MAX - b, which reaches the
   middle end as a comparison with ~b.  The strict and the non-strict form
   agree, because at a == MAX - b the sum is MAX and so is the other arm.  */

#define DEF(N, T, MX)						\
  T f1_##N (T a, T b) { return a > (T) (MX - b) ? MX : a + b; }	\
  T f2_##N (T a, T b) { return a >= (T) (MX - b) ? MX : a + b; } \
  T f3_##N (T a, T b) { return b > (T) ~a ? MX : a + b; }	\
  T f4_##N (T a, T b) { return b >= (T) ~a ? MX : a + b; }	\
  T f5_##N (T a, T b) { return a <= (T) (MX - b) ? a + b : MX; } \
  T f6_##N (T a, T b) { return a < (T) (MX - b) ? a + b : MX; }	\
  T f7_##N (T a, T b) { return (T) (MX - a) < b ? MX : a + b; }	\
  T f8_##N (T a, T b) { return (T) (MX - a) <= b ? MX : a + b; } \
  T f9_##N (T a, T b) { return (T) ~b >= a ? a + b : MX; }	\
  T f10_##N (T a, T b) { return (T) ~b > a ? a + b : MX; }

DEF (8, u8, 255)
DEF (16, u16, 65535)
DEF (32, u32, 0xffffffffu)
DEF (64, u64, ~0ull)

/* { dg-final { scan-tree-dump-times "\\.SAT_ADD " 40 "optimized" } } */
