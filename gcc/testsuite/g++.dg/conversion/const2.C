struct B {};
struct D : public B {};

typedef int B::*bm;
typedef int D::*dm;

bm bp;

void f() {
  const_cast<dm>(bp); // { dg-error "3:invalid .const_cast." }
}
