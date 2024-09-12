// { dg-do run }
// { dg-additional-options "-fno-tree-sra" }
// { dg-skip-if "requires hosted libstdc++ for complex" { ! hostedlib } }

#include <complex>

using namespace std;

class bar_src {
 public:
  bar_src() : next(0) {}
  virtual ~bar_src() { delete next; }

  bar_src *next;
};

class foo_src : public bar_src {
 public:
  foo_src(double f, double fwidth, double s = 5.0);
  virtual ~foo_src() {}

 private:
  double freq, width, peak_time, cutoff;
};


foo_src::foo_src(double f, double fwidth, double s) {
  freq = f; width = 1/fwidth; cutoff = s*width; peak_time = cutoff;
}

complex<double> do_ft2(int i) __attribute__ ((noinline));

complex<double> do_ft2(int i) {
  return i == 0 ? complex<double>(-491.697,887.05) : complex<double>(-491.692,887.026);
}

void foo(void) {
  complex<double> prev_ft = 0.0, ft = 0.0;
  for (int i=0; i < 2; i++) {
    prev_ft = ft;
    {
      foo_src src(1.0, 1.0 / 20);
      ft = do_ft2(i);
    }
    if (i > 0)
      {
        double a = abs(ft - prev_ft);
        if (a < 0.024 || a > 0.025)
          __builtin_abort ();
      }
  }
}

int main()
{
  foo();
  return 0;
}
