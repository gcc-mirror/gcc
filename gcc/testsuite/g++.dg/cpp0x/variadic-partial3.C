// PR c++/89231
// { dg-do compile { target c++11 } }

template<class... Ps>
struct A {
  template<int... Ns>
  struct Collect { };

  template<int C, int I = 0, class S = Collect<>>
  struct Seq;

  template<int C, int I, int... N>
  struct Seq<C, I, Collect<N...>> : Seq<C - 1, I + 1, Collect<N..., I>> { };

  template<int I, int... N>
  struct Seq<0, I, Collect<N...>> : Collect<N...> { };
};

A<int>::Seq<4> test;
