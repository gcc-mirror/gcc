// Origin: Jean-Marc Bourguet <bourguet@cadence.com>
// Build don't link:

class foo {
public:
  foo() {};
  void throwMe () {
    throw *this;                // ERROR - cannot be used in throw-expression
  };
  virtual void test () = 0;
};

