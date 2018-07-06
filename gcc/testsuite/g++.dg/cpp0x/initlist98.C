// PR c++/83227
// { dg-do compile { target c++11 } }

#include <initializer_list>

template <typename d> struct f {
  f(std::initializer_list<d>) {}
};

struct h {};
struct i : h {
  i();
};
void foo(f<h>);
int main() {
  foo({i{}});
}
