// Verify non-dependent assignment expressions are recognized as such
// and are checked ahead of time.
// PR c++/63198
// { dg-do compile { target c++11 } }

struct X { using t1 = int; };
struct Y { X operator=(const Y&); } y;
template<class T> void f1(decltype(y = y)::t1);

int n;
template<class T> void f2(decltype(n = n)::t1); // { dg-error "not a class" }
template<class T> void f3(decltype(n += n)::t1); // { dg-error "not a class" }

template<class T>
void g() {
  const int n;
  n = 42; // { dg-error "read-only" }

  const X x;
  x = {}; // { dg-error "no match" }

  const Y y;
  y = {}; // { dg-error "no match" }
  Y{} = X{}; // { dg-error "no match" }
}
