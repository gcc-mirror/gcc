// PR c++/95181
// { dg-do compile { target concepts } }

template <typename> struct f {
  template <typename T=int> f();
  template <typename T=int> requires false f();
};

f<int> a;
