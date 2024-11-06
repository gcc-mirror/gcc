/* { dg-do preprocess }*/
/* { dg-options "-std=c11 -pedantic-errors" { target c } } */
/* { dg-options "-std=c++17 -pedantic-errors" { target c++ } } */

#define CALL(F, ...) F (7 __VA_OPT__(,) __VA_ARGS__) /* { dg-error "'__VA_OPT__' is not available" } */
