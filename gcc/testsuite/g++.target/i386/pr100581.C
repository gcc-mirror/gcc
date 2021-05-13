/* PR target/100581 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mxop" } */

typedef float __attribute__((__vector_size__(8))) v64f32;

v64f32 af, bf, ff_a, ff_b;

v64f32 f() { return ff_a > ff_b ? af : bf; }
