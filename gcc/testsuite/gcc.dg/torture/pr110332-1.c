/* { dg-do compile } */

struct s { int lie; };
struct s si7, si8;
unsigned u9, u6;
_Bool b3, b4;
unsigned transferValues(struct s *End) {
  unsigned RegIdx;
  unsigned *t = b4 ? &u6 : &u9;
  RegIdx = *t;
  *End = *(b3 ? &si7 : &si8);
  return RegIdx;
}
