/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-march=x86-64 -mtune=generic -Og -ffinite-math-only" } */

typedef _Float128 __attribute__((__vector_size__ (16))) U;
typedef _Float128 __attribute__((__vector_size__ (32))) V;
typedef _Float16  __attribute__((__vector_size__ (16))) W;

U u;
V v;
W w;

void
foo (void)
{
    w *= (W)(u == __builtin_shufflevector (v, u, 2));
}
