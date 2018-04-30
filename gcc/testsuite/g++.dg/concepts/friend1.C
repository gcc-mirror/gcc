// { dg-options "-std=c++17 -fconcepts" }

template<typename T>
  concept bool Eq() { return requires(T t) { t == t; }; }

struct Nt {
  template<Eq T> friend void f(T) { }
} nt;

template<typename T> struct S;

template<Eq T>
  void proc(S<T>*);

template<typename T>
  struct S {
    friend bool operator==(S, S) requires Eq<T>() { return true; }

    friend void proc<>(S*); // { dg-error "does not match any template declaration" }
  };

struct X { } x;

int main() {
  // f(0); // OK
  f(nt); // { dg-error "cannot call" }
  f(x);  // { dg-error "3:'f' was not declared" }

  S<int> si;
  si == si; // OK

  S<X> sx;
  sx == sx; // { dg-error "no match" }
}
