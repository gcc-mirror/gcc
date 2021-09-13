/* PR target/98218 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2" } */

typedef unsigned int __attribute__((__vector_size__ (8))) v64u32;
typedef int __attribute__((__vector_size__ (8))) v64s32;
typedef float __attribute__((__vector_size__ (8))) v64f32;

v64u32 tu (v64f32 a, v64f32 b) { return a > b; }
v64s32 ts (v64f32 a, v64f32 b) { return a > b; }
v64f32 fu (v64u32 a, v64u32 b) { return a > b; }
v64f32 fs (v64s32 a, v64s32 b) { return a > b; }
v64f32 ff (v64f32 a, v64f32 b) { return a > b; }

/* { dg-final { scan-assembler-times "cmpltps" 3 } } */
/* { dg-final { scan-assembler-times "pcmpgtd" 2 } } */
