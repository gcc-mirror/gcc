// { dg-module-do run { target c++20 } }
// { dg-additional-options "-fmodules" }

#include <coroutine>
import M;

int main() {
  auto a = coroutine();
  a.resume();
  a.destroy();

  auto b = inline_coroutine();
  b.resume();
  b.destroy();

  auto c = template_coroutine<int>();
  c.resume();
  c.destroy();
}
