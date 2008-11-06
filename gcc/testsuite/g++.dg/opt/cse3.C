// This testcase resulted in invalid code generation on x86_64 targets
// due to a bug in fold_rtx. For a "true" value, fold_rtx represented it
// as const_true_rtx in floating-point mode, if the FLOAT_STORE_FLAG_VALUE
// macro is not defined.

// { dg-do run }
// { dg-options "-O1 -fno-guess-branch-probability -fcse-follow-jumps -fgcse -frerun-cse-after-loop" }

class StatVal {

 public:

  StatVal(double ev, double va)
    : m(ev),
      v(va) {}

  StatVal(const StatVal& other)
    : m(other.m),
      v(other.v) {}

  StatVal& operator*=(const StatVal& other) {
    double A = m == 0 ? 1.0 : v / (m * m);
    double B = other.m == 0 ? 1.0 : other.v / (other.m * other.m);
    m = m * other.m;
    v = m * m * (A + B);
    return *this;
  }

  double m;
  double v;
};

extern "C" void abort (void);

const StatVal two_dot_three(2, 0.3);

int main(int argc, char **argv) {

  StatVal product3(two_dot_three);

  product3 *= two_dot_three;

  if (product3.v > 2.5)
  {
    abort();
  }
  return 0;
}
