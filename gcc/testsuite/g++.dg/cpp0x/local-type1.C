// PR c++/51048
// { dg-do compile { target c++11 } }

template<typename X>
struct A {
  virtual void DoPush(X const& x) = 0;
  void Push(X const& x) { DoPush(x); }
};

template<typename X>
struct B : A<X> {
  using A<X>::Push;
  virtual void DoPush(X const&) { }
};

int main() {
  enum S { };
  B<S>().Push(S());
}
