// PR c++/108282
// { dg-do compile { target c++20 } }

template<class T>
concept TEST = requires { T::TT; };

struct C { };

template<class AT>
struct B {
  static void TT() requires TEST<AT>;
};

int main() {
  static_assert( !TEST<C> );
  static_assert( !TEST<B<C>> );

  B<C>::TT();  // { dg-error "no match" }
}
