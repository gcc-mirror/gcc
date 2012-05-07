struct B {
  virtual void b() {}
};

static B* f() {
  struct D : public B {
  };

  return new D;
}

B* g() {
  return f();
}
