// PR c++/66758
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <class T, class...Args>
concept bool Constructible =
  requires(Args&&...args) {
    T{ ((Args&&)(args))... };
    new T{((Args&&)(args))...};
  };

template <Constructible T> struct A { };
A<int> a;

