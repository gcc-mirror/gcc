// Test that conversions to base classes happen when calling
// operators.

// { dg-do run }

extern "C" void abort ();

struct B1;
struct B2;

B2* p;
B1* p2;

struct B1 {
  virtual void f () {}
};

struct B2 {
  int i;
  bool operator!() { if (this != p) abort (); return true; }
  operator void*() { if (this != p) abort (); return this; }
};

struct B3 : public B1, public B2 {
};

int main () {
  B3 b;
  p = (B2*) &b;
  p2 = (B1*) &b;
  bool b1 = b;
  bool b2 = !b;
}

