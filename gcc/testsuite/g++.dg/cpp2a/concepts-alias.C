// { dg-do compile { target c++2a } }

template<typename T>
concept Class = __is_class(T);

template<typename T>
  requires Class<T>
using X = T*;

// BUG: Alias templates are expanded at the point of use, regardless
// of whether or not they are dependent. This causes T* to be substituted
// without acutally checking the constraints. See the declaration of y1
// below.
template<typename T>
using Y = X<T>;

template<Class T> using Z = T*;

struct S { };

X<S> x1; // OK
X<int> x2; // { dg-error "template constraint failure" }
Y<int> y1; // { dg-error "" "" { xfail *-*-* } }
Z<S> z1; // ok

