// { dg-do run { target fpic } }
// { dg-options "-fPIC" }
// Test that non-variadic function calls using thunks and PIC work right.

struct A {
  void* p;
  A (void* q): p (q) { }
  A (const A& a): p (a.p) { }
};

class CBase {
public:
  virtual void BaseFunc();
};

class MMixin {
public:
   virtual A MixinFunc(int arg, A arg2) = 0;
};

class CExample : public CBase, public MMixin {
public:
   A MixinFunc(int arg, A arg2);
};

void CBase::BaseFunc()
{
}

A CExample::MixinFunc(int arg, A arg2)
{
  if (arg != 1 || arg2.p != 0)
    return 0;
  return this;
}

void* test(MMixin& anExample)
{
  return anExample.MixinFunc(1,A(0)).p;
}

int
main ()
{
  CExample c;

  if (test(c) != &c)
    return 1;

  return 0;
}
