// Test that explicit scope suprresses virtual lookup even after an
// explicit object.

extern "C" int printf (const char *, ...);

struct A
{
  virtual int f () { return 0; }
};

struct B: public A
{
  int f () { return 1; }
  int g() { return this->A::f(); }
};

int main()
{
  B b;
  return b.g();
}
