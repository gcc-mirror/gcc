// PR c/70297
// { dg-do compile { target c++11 } }

#define N 64

typedef int T;
typedef int T __attribute__((aligned (N)));
typedef int T __attribute__((aligned (N * 2)));
typedef int T __attribute__((aligned (N)));
typedef int T;

static_assert (alignof (T) == N * 2, "N * 2");
