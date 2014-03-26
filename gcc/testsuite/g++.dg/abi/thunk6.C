// PR c++/60566
// We need to emit the construction vtable thunk for ~C even if we aren't
// going to use it.

struct A
{
  virtual void f() = 0;
  virtual ~A() {}
};

struct B: virtual A { int i; };
struct C: virtual A { int i; ~C(); };

C::~C() {}

int main() {}

// { dg-final { scan-assembler "_ZTv0_n32_N1CD1Ev" } }
