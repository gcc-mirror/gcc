template <typename X>
class Foo {
  int i;
public:
  Foo() {
    X::explode(); // { dg-error "" }
  }
};

class Bar {
  Foo<int> foo_;
public:
  Bar() {}  // { dg-error "instantiated" }
};

template class Foo<int>;

