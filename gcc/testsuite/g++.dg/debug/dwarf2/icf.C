// Test support for ICF debugging. 
// { dg-do compile }
// { dg-options "-O0 -gdwarf-2 -fenable-icf-debug -dA" }

class A
{
 public:
  A();
  virtual void work();
  virtual int p();
 private:
  int i;
};

class B
{
 public:
  B();
  ~B();
  void work(const A* a);
 private:
  int j;
};

int
test1(A* a)
{
  a->work();
}

int
test2(A* a)
{
  if (a->p())
    {
      B b;
      b.work(a);
    }
}

// Verify that we get .debug_dcall and .debug_vcall tables generated
// and that we see entries for both virtual calls. 
// { dg-final { scan-assembler "\\.section.*\.debug_dcall" } }
// { dg-final { scan-assembler "\\.section.*\.debug_vcall" } }
// { dg-final { scan-assembler "New caller" } }
// { dg-final { scan-assembler "Caller DIE offset" } }
// { dg-final { scan-assembler "Point of call" } }
// { dg-final { scan-assembler "Callee DIE offset" } }
// { dg-final { scan-assembler "0x0.*Vtable slot" } }
// { dg-final { scan-assembler "0x1.*Vtable slot" } }
