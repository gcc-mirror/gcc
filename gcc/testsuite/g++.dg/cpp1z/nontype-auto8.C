// PR c++/79549
// { dg-do compile { target c++17 } }

template <auto...>
struct meow;

template <auto C>
struct meow<C> { };

meow<1> m;
