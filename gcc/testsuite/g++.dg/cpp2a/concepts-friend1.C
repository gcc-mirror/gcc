// { dg-do compile { target c++20 } }

template<typename T>
  concept Eq = requires(T t) { t == t; }; // { dg-message "in requirements" }

struct Nt {
  template<Eq T> friend void f(T) { }
} nt;

template<typename T> struct S;

template<Eq T>
  void proc(S<T>*);

template<typename T>
  struct S {
    friend bool operator==(S, S) requires Eq<T> { return true; }

    friend void proc<>(S*); // { dg-error "does not match any template declaration" }
  };

struct X { } x;

int main() {
  // f(0); // OK
  f(nt); // { dg-error "" }
  f(x);  // { dg-error "3:'f' was not declared" }

  S<int> si;
  si == si; // OK

  S<X> sx;
  sx == sx; // { dg-error "no match" }
}
