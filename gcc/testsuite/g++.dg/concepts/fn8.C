// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
  concept bool Class() { return __is_class(T); }

template<Class T> void f(T) { }

template<typename T> void fn(T) { }

auto p1 = &f<int>; // { dg-error "no matches" }
void (*p2)(int) = &f<int>; // { dg-error "no matches" }
void (*p3)(int) = &f; // { dg-error "no matches" }

struct S {
  template<Class T> int f(T) { }
};

auto p4 = &S::template f<int>; // { dg-error "no matches" }
int (S::*p6)(int) = &S::template f<int>; // { dg-error "no matches" }
int (S::*p7)(int) = &S::f; // { dg-error "no matches" }

template<typename T>
  void g(T x) { }

int main () {
  g(&f<int>); // { dg-error "no matches" }
}
