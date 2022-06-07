// PR c++/101906
// Like unevaluated1.C, but where the unevaluated context is a
// constraint instead of sizeof.
// { dg-do compile { target c++20 } }

template<int> using voidify = void;

template<class T>
concept constant_value_initializable
  = requires { typename voidify<(T(), 0)>; };

struct A {
  int m = -1;
};

static_assert(constant_value_initializable<A>);
