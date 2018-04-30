// PR c++/66758
// { dg-options "-std=c++17 -fconcepts" }

template <class T, class...Args>
concept bool Constructible =
  requires(Args&&...args) {
    T{ ((Args&&)(args))... };
    new T{((Args&&)(args))...};
  };

template <Constructible T> struct A { };
A<int> a;

