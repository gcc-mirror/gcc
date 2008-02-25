// PR c++/13594 (secondary)

// { dg-do compile }

namespace fool {
  inline namespace foo {
    template <class T> void swap(T, T);
  }
  template <class T> void swap(T);
}

int main() {
  // we used to fail to look up the associated namespace here
  fool::swap(1, 1);
}
