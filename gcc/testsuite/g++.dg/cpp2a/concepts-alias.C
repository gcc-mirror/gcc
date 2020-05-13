// { dg-do compile { target c++20 } }

template<typename T>
concept Class = __is_class(T);

template<typename T>
  requires Class<T>
using X = T*;

template<typename T>
using Y = X<T>;			// { dg-error "constraint" }

template<Class T> using Z = T*;

struct S { };

X<S> x1; // OK
X<int> x2; // { dg-error "template constraint failure" }
Y<int> y1; // { dg-message "" }
Z<S> z1; // ok
