// PR c++/116748

namespace ns {
  struct empty;
}

void foo() {
  using ns::empty;
  int empty;
}
