// Build don't link:
// prms-id: 10511

class S {
public:
  enum E {a, b, c};
};

class C {
public:
  bool f (S::E()) { }
  virtual void foo();
};

void C::foo() { }
