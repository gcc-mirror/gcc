// prms-id: 4246

extern "C" void abort ();
int num_d;

class A
{
 public:
  A() { }
  virtual  ~A() { }
  virtual void id() { }
};

class B
{
 public:
  B() { }
  virtual  ~B() { }
  virtual void id() { }
};

class C : public A, public B
{
 public:
  C() { }
  virtual  ~C() { }
  void id() { abort(); }
};

class D : public C
{
 public:
  D() { ++num_d; }
  virtual  ~D() { -- num_d; }
  void id() { }
};

int main()
{
  D* dp2 = new D;
  ((B*)dp2)->id();
  delete (B*) dp2;

  B* bp1 = new D;
  bp1->id();
  delete bp1;
  return num_d != 0;
}
