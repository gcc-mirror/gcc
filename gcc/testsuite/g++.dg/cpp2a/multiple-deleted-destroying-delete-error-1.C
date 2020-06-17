// { dg-do compile { target c++20 } }

#include <new>

namespace delete_selection_d {
  struct B {
    void operator delete(void*) = delete;
    void operator delete(B *, std::destroying_delete_t) = delete;
  };
  void delete_B(B *b) { delete b; }  // { dg-bogus "deleted .* deleted" }
  // { dg-error "deleted" "" { target c++20 } .-1 }
}
