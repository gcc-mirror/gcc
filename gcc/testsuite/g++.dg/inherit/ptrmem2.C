// PR c++/16810

struct C {
  virtual void f() {}
};

struct B {virtual ~B() {} };

class D : public B, public C
{
public:
  virtual void f() {}
};

typedef void ( C::*FP)();
typedef void ( D::*D_f)();

int main() {
  D *d = new D();
  C *c = d;

  const FP fptr = (FP) &D::f;;
  (d->* (D_f)fptr)();
}

