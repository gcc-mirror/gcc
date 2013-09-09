// PR c++/21682

namespace one {
  template<typename T> void fun(T);
}

using one::fun;

template<typename T> void fun(T);  // { dg-error "conflicts" }

template<typename T> void funr(T);

namespace oner {
  template<typename T> void funr(T);
}

using oner::funr;  // { dg-error "conflicts" }
