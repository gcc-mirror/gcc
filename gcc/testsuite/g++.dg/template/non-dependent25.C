// PR c++/109480

template<class T>
struct A {
  void f() {
    A<int> a;
    const bool b = a.g(); // { dg-bogus "private" }
  }

private:
  bool g() const;
};

template struct A<int>;
