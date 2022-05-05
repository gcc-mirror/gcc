// PR c++/105476
// { dg-do compile { target c++17 } }

template<class> struct Visitor_functor;

template<class> struct Events {
  template<class... Fn> struct Visitor : Visitor_functor<Fn>::type_t... { };
};

using ev_t = Events<int>;
ev_t::Visitor v = { {} }; // { dg-error "too many initializers" }
