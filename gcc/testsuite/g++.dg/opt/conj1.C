// PR target/5740
// This testcase ICEd on SPARC -m64 because emit_group_load tried
// to move a DFmode register into DImode register directly.
// { dg-do compile }
// { dg-options "-O2" }

struct C
{
  C (double y, double z) { __real__ x = y; __imag__ x = z; }
  double r () const { return __real__ x; }
  double i () const { return __imag__ x; }
  __complex__ double x;
};

C conj (const C& z)
{
  return C (z.r (), -z.i ());
}
