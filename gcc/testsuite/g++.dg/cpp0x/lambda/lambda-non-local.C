// PR c++/86499
// { dg-do compile { target c++11 } }

auto l1 = [=]{}; // { dg-error "non-local lambda expression cannot have a capture-default" }
auto l2 = [&]{}; // { dg-error "non-local lambda expression cannot have a capture-default" }

namespace {
  auto l3 = [=]{}; // { dg-error "non-local lambda expression cannot have a capture-default" }
  auto l4 = [&]{}; // { dg-error "non-local lambda expression cannot have a capture-default" }
}
