/* { dg-do compile { target { c || c++11 } } } */
/* { dg-additional-options "-std=c23" { target c } } */
/* { dg-additional-options "-fdump-tree-original" } */

constexpr int flag = 1;

void f() {
#pragma omp metadirective when(user={condition(flag)} : nothing) \
  otherwise(error at(execution))
}

/* { dg-final { scan-tree-dump-not "__builtin_GOMP_error" "original" } } */

