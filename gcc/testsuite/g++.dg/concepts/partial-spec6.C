// PR c++/67152
// { dg-options "-std=c++1z -fconcepts" }

template <class T>
concept bool HasType = requires { typename T::type; };

template<class T>
struct trait {
  using type = void;
};

struct has_type { using type = void; };

// Instantiation here
trait<has_type>::type foo() {}

// constrained version here. Type "has_type" would fail this
// constraint so this partial specialization would not have been
// selected.
template<class T>
  requires !HasType<T>
struct trait<T> {
  using type = void;
};
