// These tests cover various crashes found in development
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontract-continuation-mode=on -fcontracts-nonattr-inheritance-mode=P2900R13" }
template<typename T>
struct Base
{
  virtual int f(const int a) pre(i > 5){return a;};
private:
  int i = 2;
};

int Base::f(const int a) // { dg-error "used without template arguments" }
{
  return a;
}

namespace parsing_virtual_test {
  struct A {
    virtual void f(int i)
      pre(i >= 0);
  };

  struct B : A {
    void f(int i) override final
      pre(i >= 0);
  };

  struct C : A {
    void f(int i) override
      pre(i >= 0) = 0;
  };
}

namespace parsing_default_delete_pure_test {
  const bool a = true, b = true, c = true;

  struct X {
    X() pre(a) = default;
    X(const X&) pre(b) = delete;
    virtual void f() pre(c) = 0;
  };
}
