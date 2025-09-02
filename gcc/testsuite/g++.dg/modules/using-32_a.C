// PR c++/120195
// { dg-additional-options "-fmodules" }

export module M;

extern "C++" void foo() {}
export using ::foo;

namespace ns {
  extern "C" void bar() {}
}
extern "C" void bar();
export using ns::bar;
