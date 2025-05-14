// PR c++/118629
// { dg-do compile { target c++11 } }

void foo() {
  []() -> decltype(+__FUNCTION__) { return nullptr; };
}
