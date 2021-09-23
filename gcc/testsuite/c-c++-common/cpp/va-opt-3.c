/* PR preprocessor/83063 */
/* PR preprocessor/83708 */
/* { dg-do preprocess } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "-std=c++2a" { target c++ } } */

#define f1(...) b##__VA_OPT__(c)
#define f2(...) __VA_OPT__(c)##d
#define f3(...) e##__VA_OPT__()
#define f4(...) __VA_OPT__()##f
#define f5(...) g##__VA_OPT__(h)##i
#define f6(...) j##__VA_OPT__()##k
#define f7(...) l##__VA_OPT__()
#define f8(...) __VA_OPT__()##m
#define f9(...) n##__VA_OPT__()##o
#define f10(x, ...) x##__VA_OPT__(x)
#define f11(x, ...) __VA_OPT__(x)##x
#define f12(x, ...) x##__VA_OPT__(x)##x
#define f13(...) __VA_OPT__(a)__VA_OPT__(b)c
#define f14(a, b, c, ...) __VA_OPT__(a)__VA_OPT__(b)c
#define f15(a, b, c, ...) __VA_OPT__(a b)__VA_OPT__(b c)a/**/__VA_OPT__(c a)a
#define m1 (
#define f16() f17 m1 )
#define f17() f18 m1 )
#define f18() m2 m1 )
#define m3f17() g
#define f19(x, ...) m3 ## __VA_OPT__(x x f16() #x)
#define f20(x, ...) __VA_OPT__(x x)##m4()
#define f21() f17
#define f17m4() h
#define f22(x,...)  1 ## __VA_OPT__(x ## x 2) ## 3
#define f23(x,...)  1 ## __VA_OPT__(x 2) ## 3
#define f24(x,...)  1 ## __VA_OPT__(2 x) ## 3
#define f25(x,...)  1 ## __VA_OPT__(2 x ## x) ## 3
t1 f1 (1, 2, 3);
/* { dg-final { scan-file va-opt-3.i "t1 bc;" } } */
t2 f1 ();
/* { dg-final { scan-file va-opt-3.i "t2 b;" } } */
t3 f2 (1, 2);
/* { dg-final { scan-file va-opt-3.i "t3 cd;" } } */
t4 f2 ();
/* { dg-final { scan-file va-opt-3.i "t4 d;" } } */
t5 f3 (1);
/* { dg-final { scan-file va-opt-3.i "t5 e;" } } */
t6 f4 (2);
/* { dg-final { scan-file va-opt-3.i "t6 f;" } } */
t7 f5 (6, 7);
/* { dg-final { scan-file va-opt-3.i "t7 ghi;" } } */
t8 f5 ();
/* { dg-final { scan-file va-opt-3.i "t8 gi;" } } */
t9 f6 (8);
/* { dg-final { scan-file va-opt-3.i "t9 jk;" } } */
t10 f7 ();
/* { dg-final { scan-file va-opt-3.i "t10 l;" } } */
t11 f8 ();
/* { dg-final { scan-file va-opt-3.i "t11 m;" } } */
t12 f9 ();
/* { dg-final { scan-file va-opt-3.i "t12 no;" } } */
t13 f10 (p, 5, 6);
/* { dg-final { scan-file va-opt-3.i "t13 pp;" } } */
t14 f10 (p);
/* { dg-final { scan-file va-opt-3.i "t14 p;" } } */
t15 f11 (q, 7);
/* { dg-final { scan-file va-opt-3.i "t15 qq;" } } */
t16 f11 (q);
/* { dg-final { scan-file va-opt-3.i "t16 q;" } } */
t17 f12 (r, 1, 2, 3, 4, 5);
/* { dg-final { scan-file va-opt-3.i "t17 rrr;" } } */
t18 f12 (r);
/* { dg-final { scan-file va-opt-3.i "t18 rr;" } } */
t19 f13 (1, 2);
/* { dg-final { scan-file va-opt-3.i "t19 a b c;" } } */
t20 f13 ();
/* { dg-final { scan-file va-opt-3.i "t20 c;" } } */
t21 f14 (3, 4, 5, 2);
/* { dg-final { scan-file va-opt-3.i "t21 3 4 5;" } } */
t22 f14 (3, 4, 5);
/* { dg-final { scan-file va-opt-3.i "t22 5;" } } */
t23 f15 (6, 7, 8, 9);
/* { dg-final { scan-file va-opt-3.i "t23 6 7 7 8 6 8 6 6;" } } */
t24 f15 (6, 7, 8);
/* { dg-final { scan-file va-opt-3.i "t24 6 6;" } } */
t25 f19 (f16 (), 1);
/* { dg-final { scan-file va-opt-3.i {t25 g f18 \( \) f17 \( \) "f16 \(\)";} } } */
t26 f20 (f21 (), 2);
/* { dg-final { scan-file va-opt-3.i "t26 f17 h;" } } */
t27 f22 (, x);
/* { dg-final { scan-file va-opt-3.i "t27 1 23;" } } */
t28 f23 (, x);
/* { dg-final { scan-file va-opt-3.i "t28 1 23;" } } */
t29 f24 (, x);
/* { dg-final { scan-file va-opt-3.i "t29 12 3;" } } */
t30 f25 (, x);
/* { dg-final { scan-file va-opt-3.i "t30 12 3;" } } */
