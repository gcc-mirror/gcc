// PR c++/86981
// { dg-do compile { target c++17 } }
// { dg-options "-Wpessimizing-move" }

#include <utility>
#include <optional>

struct T {
  T() { }
  T(const T&) { }
  T(T&&) { }
};

std::optional<T>
fn ()
{
  T t;
  return std::move (t);
}
