// PR c++/80562
// { dg-do compile { target c++17 } }

struct T {
  int i;
  constexpr auto foo() { return false; }
};

template <class MustBeTemplate>
constexpr auto bf(T t) {
    if constexpr(t.foo()) {
        return false;
    }
    return true;
}
