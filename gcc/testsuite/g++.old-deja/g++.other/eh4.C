// { dg-do assemble  }
// Origin: Jean-Marc Bourguet <bourguet@cadence.com>

class foo {
public:
  foo() {}
  void throwMe () {
    throw *this;                // { dg-error "" } cannot be used in throw-expression
  }
  virtual void test () = 0;
};

