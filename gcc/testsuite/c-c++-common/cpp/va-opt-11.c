/* PR preprocessor/125048 */
/* { dg-do preprocess } */
/* { dg-options "-std=c23" { target c } } */
/* { dg-options "-std=c++20" { target c++ } } */

#define A(...)B(B(B(B(__VA_ARGS__##__VA_OPT__()))))
#define B(...)C(C(C(C(__VA_ARGS__##__VA_OPT__()))))
#define C(...)D(D(D(D(__VA_ARGS__##__VA_OPT__()))))
#define D(...)E(E(E(E(__VA_ARGS__##__VA_OPT__()))))
#define E(...)__VA_ARGS__
#define F
A(F)
/* { dg-final { scan-file va-opt-11.i "D\\\(D\\\(D\\\(C\\\(C\\\(C\\\(B\\\(B\\\(B\\\(\\\)\\\)\\\)\\\)\\\)\\\)\\\)\\\)\\\)" } } */
