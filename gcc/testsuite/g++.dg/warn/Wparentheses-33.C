// PR c++/112765
// { dg-additional-options "-Wparentheses" }

struct A {
  A& operator=(const A&);
  operator bool() const;
};

template<class T>
void f(A a1, A a2) {
  if ((a2 = a1)) // { dg-bogus "parentheses" }
    return;
  bool b = (a2 = a1); // { dg-bogus "parentheses" }
}

template void f<int>(A, A);

template<class T>
void g(T a1, T a2) {
  if ((a2 = a1)) // { dg-bogus "parentheses" }
    return;
  bool b = (a2 = a1); // { dg-bogus "parentheses" }
}

template void g<A>(A, A);
