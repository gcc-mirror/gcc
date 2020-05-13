// PR c++/90171
// { dg-do compile { target c++20 } }

#include <new>

struct A {
  void operator delete(A*, std::destroying_delete_t, std::align_val_t);
  void operator delete(A*, std::destroying_delete_t, std::size_t, std::align_val_t);
};

void delete_A(A *a) { delete a; }
