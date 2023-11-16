// PR c++/104802
// { dg-do compile { target c++17 } }

template<auto const& ... Args>
struct S {
  template<typename=void>
  void operator()() const {}
};

struct weird_ {
  int operator&() const { return 123; }
} const weird {};

int main() {
  S<weird> s {};
  s();
}
