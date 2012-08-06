// { dg-do compile }
// { dg-options "-g2 -dA" }

// Verify that line number info is output for the non-virtual
// thunks for C::~C().
// { dg-final { scan-assembler "thunk.C:30" } }

class A
{
 public:
  A();
  virtual ~A();
 private:
  int i;
};

class B
{
 public:
  B();
  virtual ~B();
 private:
  int i;
};

class C : public A, public B
{
 public:
  C();
  virtual ~C(); // line 30
};

C::C()
{
}

C::~C()
{
}
