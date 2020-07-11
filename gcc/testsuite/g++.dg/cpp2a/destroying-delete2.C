// PR c++/88969
// { dg-do compile { target c++20 } }

#include <new>

namespace delete_selection_d {
  struct B {
    void operator delete(void*) = delete;
    void operator delete(B *, std::destroying_delete_t) = delete;  // { dg-message "declared here" }
  };
  void delete_B(B *b) { delete b; }  // { dg-error "use of deleted function" }
}

namespace delete_selection_r {
  struct B {
    void operator delete(B *, std::destroying_delete_t) = delete;  // { dg-message "declared here" }
    void operator delete(void*) = delete;
  };
  void delete_B(B *b) { delete b; }  // { dg-error "use of deleted function" }
}
