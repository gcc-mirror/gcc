// PR c++/112288
// { dg-do link }
// A version of friend81.C where A and B have a different number of template
// parameters.

template<class T> struct A;
template<class T, class = void> struct B;

A<int>* a;
B<int>* b;

template<class T, class>
struct B {
  template<class U>
  friend int f(A<T>*, B*, U);
};

template struct B<int>; // f declared
int n = f(a, b, 0); // f<int> specialized

template<class T>
struct A {
  template<class U>
  friend int f(A*, B<T>*, U) { return 42; }
};

template struct A<int>; // f defined, need to make existing f<int> point to defn
int m = f(a, b, 0); // reuses existing specialization f<int>

int main() { }
