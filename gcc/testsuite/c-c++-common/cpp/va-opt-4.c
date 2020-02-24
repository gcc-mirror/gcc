/* PR preprocessor/92319 */
/* { dg-do preprocess } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "-std=c++2a" { target c++ } } */

#define f1(...) b##__VA_OPT__(c)
#define e
#define e2 e
#define e3 1
#define e5 e3
t1 f1 (e);
/* { dg-final { scan-file va-opt-4.i "t1 b;" } } */
t2 f1 (e2);
/* { dg-final { scan-file va-opt-4.i "t2 b;" } } */
t3 f1 (e3);
/* { dg-final { scan-file va-opt-4.i "t3 bc;" } } */
t4 f1 (e4);
/* { dg-final { scan-file va-opt-4.i "t4 bc;" } } */
t5 f1 (e5);
/* { dg-final { scan-file va-opt-4.i "t5 bc;" } } */
