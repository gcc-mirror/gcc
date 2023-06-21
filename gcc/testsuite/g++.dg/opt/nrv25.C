// PR c++/58487
// { dg-additional-options -Wnrvo }

struct A {
  A() {}
  A(const A&);
};

A test() {
  A a, b;
  if (true)
    return a;
  else
    return b;			// { dg-warning Wnrvo }
}
