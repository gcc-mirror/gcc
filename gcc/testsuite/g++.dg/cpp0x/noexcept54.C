// PR c++/66256 - noexcept-specifier is a complete-class context.
// { dg-do compile { target c++11 } }

void swap(int&, int&);

int& get();

struct pair {
  void swap(pair&) noexcept(noexcept(swap(get(), get()))) { } // { dg-error "no matching function for call" }
};
