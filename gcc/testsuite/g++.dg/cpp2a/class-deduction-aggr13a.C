// PR c++/105476
// { dg-do compile { target c++20 } }
// A valid version of class-deduction-aggr13.C.

template<class> struct Visitor_functor;

template<> struct Visitor_functor<int> {
  using type_t = int;
};

template<class T> struct Events {
  template<class Fn=T> struct Visitor {
    Visitor_functor<Fn>::type_t t;
  };
};

using ev_t = Events<int>;
ev_t::Visitor v = { {} };
