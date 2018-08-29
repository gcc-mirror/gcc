// PR c++/86981
// { dg-options "-Wpessimizing-move -std=c++17" }

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
