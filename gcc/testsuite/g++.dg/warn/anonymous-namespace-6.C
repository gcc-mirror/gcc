// PR c++/53711
// { dg-options -Wall }

namespace {
  void f () // { dg-warning "not used" }
  {
  }
}
