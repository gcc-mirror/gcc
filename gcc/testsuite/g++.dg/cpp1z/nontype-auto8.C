// PR c++/79549
// { dg-options -std=c++17 }

template <auto...>
struct meow;

template <auto C>
struct meow<C> { };

meow<1> m;
