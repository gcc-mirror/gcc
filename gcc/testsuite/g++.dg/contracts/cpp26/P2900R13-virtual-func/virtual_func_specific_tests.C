// generic assert contract parsing checks
//   check omitted, 'default', 'audit', and 'axiom' contract levels parse
//   check that all concrete semantics parse
//   check omitted, '%default' contract roles parse
//   ensure that an invalid contract level 'invalid' errors
//   ensure that a predicate referencing an undefined variable errors
//   ensure that a missing colon after contract level errors
//   ensure that an invalid contract role 'invalid' errors
//   ensure that a missing colon after contract role errors
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontracts-on-virtual-functions=P2900R13" }

struct B{

  virtual void f() post(true) = 0;

};

struct D : B {
  void f() override post(true) = 0;
};

// non attribute contracts come after override.
struct E : D {
  void f() post(true) override;  // { dg-error "expected" }
  // { dg-error "override" "" { target *-*-* } .-1 }
};


namespace other{


  struct X
  {
    virtual void f(int x) pre(x >= 0);
  };

  struct Y : X
  {
    void f(int x) override pre(x >= 0);
  };

  struct Y2 : X
  {
    void f(int x) pre(x >= 0) override; // { dg-error "expected .;. at end of member declaration|does not name a type" }
  };

  struct Y3 : X
  {
    void f(int x) pre(x >= 0) override pre(x >= 0); // { dg-error "expected .;. at end of member declaration|does not name a type" }
  };

  struct X2
  {
    virtual void f(int x) pre(x >= 0);
  };

  struct X3
  {
    virtual void f(int x) final pre(x >= 0);
  };

  struct X4
  {
    virtual void f(int x) pre(x >= 0) final; // { dg-error "expected .;. at end of member declaration|does not name a type" }
  };

  struct X5
  {
    virtual void f(int x) pre(x >= 0) final pre(x >= 0); // { dg-error "expected .;. at end of member declaration|does not name a type" }
  };


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

int main()
{
}
