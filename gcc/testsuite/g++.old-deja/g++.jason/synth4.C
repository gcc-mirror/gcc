// { dg-do assemble  }

struct X {
  X();
};
typedef	void (X::*mfp)();
struct Y {
  Y();
  mfp memfp;
};
void f()
{
  Y *y1, *y2 ;
  *y1 = *y2;		 // { dg-bogus "" } failed to synthesize op=
}
