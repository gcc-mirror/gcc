// { dg-do run }
// { dg-options "-w" }

extern "C" void abort ();

struct B;

B* b;

struct A {
  virtual void f () {}
};

struct B : virtual public A {
  B () {
    b = this;
    ((A*) this)->f ();
  }

  virtual void f () {
    if (this != b)
      abort ();
  }
};

struct C : public B {
};

struct D : public C, public B {
  virtual void f () {}
};

int main () {
  D d;
}

