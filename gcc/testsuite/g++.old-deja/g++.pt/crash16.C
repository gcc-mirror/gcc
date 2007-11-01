// { dg-do assemble }
// { dg-xfail-if "" { xstormy16-*-* } { "*" } { "" } }
// { dg-options "" }

extern "C" void qsort(void *base, __SIZE_TYPE__ nmemb, __SIZE_TYPE__ size,
              int (*compar)(const void *, const void *));

struct R {
  int count;
  int state1;
  int state2;
};

int cmp_d(const R* a, const R* b) {
  return a->count > b->count;
}

namespace CXX {
  template<class T, int i1, int i2>
    inline void qsort (T b[i1][i2], int (*cmp)(const T*, const T*)) {
    ::qsort ((void*)b, i1*i2, sizeof(T), (int (*)(const void *, const void *))cmp);
  }
}

using namespace CXX;

void sort_machine() {
  struct R d[256][256];
  qsort<R,256> (d, cmp_d);
}
