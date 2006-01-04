// PR c++/25492

class Base {
public:
  class Nested {};
};

class Derived:public Base {
public:
  class Nested {
  public:
    void m();
  };
  class AnotherNested {
    friend class Nested;
    AnotherNested() {}
  };
};

void Derived::Nested::m() {
  Derived::AnotherNested instance;

}
