// prms-id: 4736

int did_fail = 1;

class O {
public:
  virtual void of() { }
};

class A : public O {
public:
  virtual void vf() { }
};

class W {
public:
  virtual void vf() { }
};

class X : public W, public A {
public:
  virtual void vf() { }
};

class Z : public X {
public:
  virtual void vf() { did_fail = 0; }
};

Z sz;

void fail1(W* w) {
  w->vf();
}

int main() {
  fail1 (&sz);
  return did_fail;
}
