double gl;

#define test(N, CMP) \
  void   N ## _dead(double a) { if (a CMP 0.0) gl = 1; } \
  double N ## _live(double a) { if (a CMP 0.0) gl = 1; return a; }

test(eq, ==)
test(ne, !=)
test(ge, >=)
test(gt, >)
test(le, <=)
test(lt, <)
