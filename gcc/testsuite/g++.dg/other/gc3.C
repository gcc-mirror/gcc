// PR c++/21687
// { dg-options "--param ggc-min-expand=0 --param ggc-min-heapsize=0" }

template <class Union>
void perform_test_trivial() {
  struct check_union {  void perform_test_trivial() {} };
}

