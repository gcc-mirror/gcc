// PR c++/100983
// { dg-do compile { target c++17 } }

struct X {
  template<int N>
  struct Y { template<class... Ts> Y(Ts...); };

  template<class... Ts> Y(Ts...) -> Y<sizeof...(Ts)>;
};

X::Y y{1,2,3};
