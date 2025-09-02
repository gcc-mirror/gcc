// PR c++/68679
// { dg-additional-options "-Wno-non-c-typedef-for-linkage" }

typedef struct {
  struct {
    unsigned d[4];
    template<typename T>
    unsigned operator[] (T i) const { return d[i]; }
  } c;
} A;
