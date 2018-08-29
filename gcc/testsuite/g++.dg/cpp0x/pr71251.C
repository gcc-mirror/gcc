// { dg-do compile { target c++11 } }

template<int,int> template<typename>
using U = void; // { dg-error "too many" }

template<typename>
using V = void;

template<typename> struct X {
  template<typename> template<typename>
  using U = void; // { dg-error "too many" }

  template<typename>
  using V = void;
};
