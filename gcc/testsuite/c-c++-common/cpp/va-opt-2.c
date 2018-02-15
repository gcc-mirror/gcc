/* PR preprocessor/83063 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "-std=c++2a" { target c++ } } */

#define f1(...) int b##__VA_OPT__(c)
#define f2(...) int __VA_OPT__(c)##d
#define f3(...) int e##__VA_OPT__()
#define f4(...) int __VA_OPT__()##f
#define f5(...) int g##__VA_OPT__(h)##i
#define f6(...) int j##__VA_OPT__()##k
#define f7(...) int l##__VA_OPT__()
#define f8(...) int __VA_OPT__()##m
#define f9(...) int n##__VA_OPT__()##o
#define f10(x, ...) int x##__VA_OPT__(x)
#define f11(x, ...) int __VA_OPT__(x)##x
#define f12(x, ...) int x##__VA_OPT__(x)##x
f1 (1, 2, 3);
f1 ();
f2 (1, 2);
f2 ();
f3 (1);
f4 (2);
f5 (6, 7);
f5 ();
f6 (8);
f7 ();
f8 ();
f9 ();
f10 (p, 5, 6);
f10 (p);
f11 (q, 7);
f11 (q);
f12 (r, 1, 2, 3, 4, 5);
f12 (r);

int
main ()
{
  return bc + b + cd + d + e + f + ghi + gi + jk + l + m + no + pp + p + qq + q + rrr + rr;
}
