// PR c++/94592 - ICE with { } as template argument.
// { dg-do compile { target c++20 } }

struct A {
  constexpr A() {}
};

template <A> struct B { };

template<typename> void bar () {
    B<{}> var;
}

void fu() {
    bar<int>();
}
