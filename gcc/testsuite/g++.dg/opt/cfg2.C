// PR optimization/12215
// Origin: <nick@ilm.com>
// Reduced testcase by Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// This used to fail because the CSE pass destroyed the CFG in presence
// of trapping loads, which led to the deletion of basic blocks.

// { dg-do compile }
// { dg-options "-O2 -fno-gcse -fnon-call-exceptions" }


struct B {
  ~B() throw() {}
};

struct X {
  X(const char*, const B&);
  ~X() {}
};

bool m();
void f(int &i, float &arg0);

void g (const char **argv) {
  float val;
  int i = 1;

  try {
    while ( i < 1 )
      {
        X arg(argv[i], B());
        if (m())
          throw(0);

        f(i, val);
      }
  } catch (...) {}
}
