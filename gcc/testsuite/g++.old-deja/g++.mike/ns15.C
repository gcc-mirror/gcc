// Build don't link:
// excess errors test - XFAIL xstormy16-*-*

#include <stdio.h>
#include <stdlib.h>

#define MAX 256
#define MAXSTATE 1000000

struct R {
  int count;
  int state1;
  int state2;
};

int cmp_d(const R* a, const R* b) {
  return a->count > b->count;
}

namespace CXX {
  template<class T, long i1, long i2>
    inline void qsort (T b[i1][i2], int (*cmp)(const T*, const T*)) {
    ::qsort ((void*)b, i1*i2, sizeof(T), (int (*)(const void *, const void *))cmp);
  }
}

using namespace CXX;

void sort_machine() {
  struct R d[MAX][MAX];
  qsort<R,MAX> (d, cmp_d);
}
