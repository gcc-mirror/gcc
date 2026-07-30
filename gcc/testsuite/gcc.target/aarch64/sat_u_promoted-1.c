/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;

/* The integer promotions turn a narrow saturating add or subtract written
   as a clamp into a widened operation feeding a MIN or a MAX.  */

u8 sub8 (u8 a, u8 b) { int t = a - b; return t < 0 ? 0 : t; }
u8 sub8_swapped (u8 a, u8 b) { int t = a - b; return t > 0 ? t : 0; }
u16 sub16 (u16 a, u16 b) { int t = a - b; return t < 0 ? 0 : t; }
u32 sub32 (u32 a, u32 b) { long long t = (long long) a - b; return t < 0 ? 0 : t; }

u8 add8 (u8 a, u8 b) { int t = a + b; return t > 255 ? 255 : t; }
u16 add16 (u16 a, u16 b) { int t = a + b; return t > 65535 ? 65535 : t; }
u32 add32 (u32 a, u32 b) { u64 t = (u64) a + b; return t > 0xffffffffu ? 0xffffffffu : t; }

u8 add8_shared (u8 a, u8 b, int *p)
{
  int t = a + b;
  *p = t;
  return t > 255 ? 255 : t;
}

/* { dg-final { scan-tree-dump-times "\\.SAT_SUB " 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\.SAT_ADD " 4 "optimized" } } */
