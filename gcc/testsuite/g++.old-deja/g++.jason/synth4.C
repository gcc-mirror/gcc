// Build don't link:

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
  *y1 = *y2;		 // gets bogus error - failed to synthesize op=
}
