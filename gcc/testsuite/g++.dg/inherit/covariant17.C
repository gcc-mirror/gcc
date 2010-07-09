// PR c++/43120
// { dg-do run }

extern "C" void abort ();

struct A {
  int a;

  A(int a_) : a(a_) {}

  A(const A &other) { }

  virtual void dummy() {}
};

struct B {
  virtual B *clone() const = 0;
};

struct C : public virtual B {
  virtual B *clone() const = 0;
};

struct E* ep;
struct E : public A, public C {
  E(int a_) : A(a_) { ep = this; }

  virtual E *clone() const {
    if (this != ep)
      abort();
    return new E(*this);
  }
};

int main() {
  E *a = new E(123);
  B *c = a;
  B *d = c->clone();
  return 0;
}
