/* { dg-do compile } */

_Bool a;
struct s { int t; } c, d;
unsigned e, f;
unsigned transferValues(struct s *End) {
  unsigned RegIdx = *(a ? &e : &f);
  *End = *(a ? &c : &d);
  return RegIdx;
}
