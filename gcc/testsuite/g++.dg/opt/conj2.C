// PR target/6043
// This testcase ICEd on IA-64 because emit_group_store
// did not handle loading CONCAT from register group
// { dg-do compile }

struct C
{
  C (double y, double z) { __real__ x = y; __imag__ x = z; }
  double r () const { return __real__ x; }
  double i () const { return __imag__ x; }
  __complex__ double x;
};

inline C conj (const C& x)
{
  return C (x.r (), - x.i ());
}

void foo (void)
{
  C x (1.0, 1.0);
  conj (x);
}
