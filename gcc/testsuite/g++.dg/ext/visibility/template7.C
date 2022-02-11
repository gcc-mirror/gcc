// PR c++/35688
// { dg-require-visibility "" }
// { dg-options "-fvisibility=hidden -fno-inline" }

// { dg-final { scan-hidden "_ZN1s6vectorI1AEC1Ev" } }
// { dg-final { scan-hidden "_ZN1s3fooI1AEEvT_" } }

namespace s __attribute__((visibility("default"))) {
  template <class T>
    class vector {
  public:
    vector() { }
  };
  template <class T>
    void foo(T t) {
  }
}

class A {
public:
  A() { }
};

s::vector<A> v;

int main() {
  A a;
  s::foo(a);
}
