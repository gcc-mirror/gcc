// PR c++/13594 (secondary)

// { dg-do compile }

namespace foo {
  template <class T> void swap(T, T);
}
namespace fool {
  using namespace foo __attribute__((strong));
  template <class T> void swap(T);
}

int main() {
  // we used to fail to look up the associated namespace here
  fool::swap(1, 1);
}
