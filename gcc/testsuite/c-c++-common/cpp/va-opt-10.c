/* PR preprocessor/105732 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "-std=c++20" { target c++ } } */

#define m1(p1, p2, p3) p3
#define m2(p1, ...) 1##__VA_OPT__(foo)
#define m3(...) m1(1, 2, m2)
#define m4(p1, ...) 1 __VA_OPT__()
#define m5(...) m1(1, 2, m4)
#if m3(,)(,)
#else
#error
#endif
#if m5(,)(,)
#else
#error
#endif
