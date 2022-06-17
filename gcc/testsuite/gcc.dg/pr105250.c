/* { dg-do compile } */
/* { dg-options "-w -Wno-psabi -O2" } */
/* { dg-skip-if "PR105266" { powerpc*-*-* s390*-*-* } } */

typedef int __attribute__((__vector_size__(4))) T;
typedef int __attribute__((__vector_size__(8))) U;
typedef int __attribute__((__vector_size__(16))) V;
typedef int __attribute__((__vector_size__(32))) W;
typedef _Float32 __attribute__((__vector_size__(16))) F;
typedef _Float64 __attribute__((__vector_size__(32))) G;
void foo();

foo(int, int, int, int, U, U, V, V, W, W, int,
     T, int, U, U, V, V, W, W, T,
     T, int, U, U, V, V, W, W, T,
     T, int, W, W, T, T, int, int, int,
     int, int, int, W, int, int, int, int, int, int,
     V, W, T, int, int, U, F, int, int, int,
     int, int, int, G)
{
  foo(0, 0, 0, 0, (U){}, (U){}, (V){}, (V){}, (W){},
       (W){}, 2, (T){}, 0, 0, 0, 0, (U){}, (U){},
       (V){}, (V){}, (W){}, (W){}, (T){},
       (T){}, 0, 0, 0, 0, (U){}, (U){}, (V){},
       (V){}, (W){}, (W){}, (T){}, (T){}, 0, 0, 0,
       0, 0, 0, (T){},
       (T){}, (W){},
       (W){}, (T){}, (T){}, 0, 0, 0, 0, 0, 0, (W){},
       (V){}, (W){}, (T){}, 0, 0, (U){}, (F){});
}
