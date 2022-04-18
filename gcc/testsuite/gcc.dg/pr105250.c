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

foo(int p1, int p2, int p3, int p4, U p5, U p6, V p7, V p8, W p9, W p10, int p11,
     T p12, int p13, U p14, U p15, V p16, V p17, W p18, W p19, T p20,
     T p21, int p22, U p23, U p24, V p25, V p26, W p27, W p28, T p29,
     T p30, int p31, W p32, W p33, T p34, T p35, int p36, int p37, int p38,
     int p39, int p40, int p41, W p42, int p43, int p44, int p45, int p46, int p47, int p48,
     V p49, W p50, T p51, int p52, int p53, U p54, F p55, int p56, int p57, int p58,
     int p59, int p60, int p61, G p62)
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
