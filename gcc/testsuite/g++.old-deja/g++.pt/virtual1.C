  struct V { virtual ~V() {} };
  template <class T> struct A : virtual V { };
  template <class T> struct B {
    virtual void f() { T foo; }
  };
  int main() { B< A<int> > bar; }
