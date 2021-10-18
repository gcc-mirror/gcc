/* PR preprocessor/101488 */
/* { dg-do preprocess } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "-std=c++2a" { target c++ } } */

#define f0() n
#define f1(x,...) a ## __VA_OPT__ (a) ## a
#define f2(x,...) a ## __VA_OPT__ () ## a
#define f3(x,...) a ## __VA_OPT__ (x) ## a
#define f4(x,...) a ## __VA_OPT__ (x##x) ## a
#define f5(x,...) a ## __VA_OPT__ (x##x 1) ## a
#define f6(x,...) a ## __VA_OPT__ (1 x##x) ## a
#define f7(x,...) __VA_OPT__ (f0 x ## x ) ## 1
#define f8(x,...) __VA_OPT__ (f0 x) ## 1
#define f9(x,...) f0 ## __VA_OPT__ (x 1) ## 1
#define f10(x,...) f0 ## __VA_OPT__ (x ## x 1) ## 1
#define f11(x, ...) __VA_OPT__(a x ## x) ## b
#define f12(x, ...) a ## __VA_OPT__(x ## x b)
#define f13(x) x ## x b
#define ab def
#define bc ghi
#define abc jkl
#define f14(x, ...) a ## __VA_OPT__(x b x) ## c
t1 f1(,);
/* { dg-final { scan-file va-opt-7.i "t1 aa;" } } */
t2 f1(,1);
/* { dg-final { scan-file va-opt-7.i "t2 aaa;" } } */
t3 f1(2,1);
/* { dg-final { scan-file va-opt-7.i "t3 aaa;" } } */
t4 f2(,);
/* { dg-final { scan-file va-opt-7.i "t4 aa;" } } */
t5 f2(,1);
/* { dg-final { scan-file va-opt-7.i "t5 aa;" } } */
t6 f2(2,1);
/* { dg-final { scan-file va-opt-7.i "t6 aa;" } } */
t7 f3(,);
/* { dg-final { scan-file va-opt-7.i "t7 aa;" } } */
t8 f3(,1);
/* { dg-final { scan-file va-opt-7.i "t8 aa;" } } */
t9 f3(2,1);
/* { dg-final { scan-file va-opt-7.i "t9 a2a;" } } */
t10 f4(,);
/* { dg-final { scan-file va-opt-7.i "t10 aa;" } } */
t11 f4(,1);
/* { dg-final { scan-file va-opt-7.i "t11 aa;" } } */
t12 f4(2,1);
/* { dg-final { scan-file va-opt-7.i "t12 a22a;" } } */
t13 f5(,);
/* { dg-final { scan-file va-opt-7.i "t13 aa;" } } */
t14 f5(,1);
/* { dg-final { scan-file va-opt-7.i "t14 a 1a;" } } */
t15 f5(2,1);
/* { dg-final { scan-file va-opt-7.i "t15 a22 1a;" } } */
t16 f6(,);
/* { dg-final { scan-file va-opt-7.i "t16 aa;" } } */
t17 f6(,1);
/* { dg-final { scan-file va-opt-7.i "t17 a1 a;" } } */
t18 f6(2,1);
/* { dg-final { scan-file va-opt-7.i "t18 a1 22a;" } } */
t19 f7(,);
/* { dg-final { scan-file va-opt-7.i "t19 1;" } } */
t20 f7(,1);
/* { dg-final { scan-file va-opt-7.i "t20 f0 1;" } } */
t21 f7(2,1);
/* { dg-final { scan-file va-opt-7.i "t21 f0 221;" } } */
t22 f8(,);
/* { dg-final { scan-file va-opt-7.i "t22 1;" } } */
t23 f8(,1);
/* { dg-final { scan-file va-opt-7.i "t23 f0 1;" } } */
t24 f8(2,1);
/* { dg-final { scan-file va-opt-7.i "t24 f0 21;" } } */
t25 f9(,);
/* { dg-final { scan-file va-opt-7.i "t25 f01;" } } */
t26 f9(,1);
/* { dg-final { scan-file va-opt-7.i "t26 f0 11;" } } */
t27 f9(2,1);
/* { dg-final { scan-file va-opt-7.i "t27 f02 11;" } } */
t28 f10(,);
/* { dg-final { scan-file va-opt-7.i "t28 f01;" } } */
t29 f10(,1);
/* { dg-final { scan-file va-opt-7.i "t29 f0 11;" } } */
t30 f10(2,1);
/* { dg-final { scan-file va-opt-7.i "t30 f022 11;" } } */
t31 f11(,);
/* { dg-final { scan-file va-opt-7.i "t31 b;" } } */
t32 f11(,1);
/* { dg-final { scan-file va-opt-7.i "t32 a b;" } } */
t33 f11(2,1);
/* { dg-final { scan-file va-opt-7.i "t33 a 22b;" } } */
t34 f12(,);
/* { dg-final { scan-file va-opt-7.i "t34 a;" } } */
t35 f12(,1);
/* { dg-final { scan-file va-opt-7.i "t35 a b;" } } */
t36 f12(2,1);
/* { dg-final { scan-file va-opt-7.i "t36 a22 b;" } } */
t37 f14(,);
/* { dg-final { scan-file va-opt-7.i "t37 ac;" } } */
t38 f14(,1);
/* { dg-final { scan-file va-opt-7.i "t38 a b c;" } } */
t39 f14(f13(),1);
/* { dg-final { scan-file va-opt-7.i "t39 def b ghi;" } } */
