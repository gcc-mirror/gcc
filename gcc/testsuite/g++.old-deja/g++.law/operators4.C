// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for complex" { ! hostedlib } }
// GROUPS passed operators
// opr-del file
// From: rollins@bart.ee.queensu.ca (Mark Rollins)
// Date:     Thu, 3 Sep 1992 22:01:03 -0400 Fri, 4 Sep 1992 02:00:25 GMT
// Subject:  delete [size] pointer; Problem
// Message-ID: <92Sep3.220137edt.30@jarvis.csri.toronto.edu>

#include <complex>
typedef std::complex<double> Complex;
#include <stdio.h>

class Vector {
  int           size;
  Complex       *v;
 public:
  Vector(int s=1) { size = s; v = new Complex[size];}
  ~Vector() { delete [size] v;}// { dg-error "" }  warning
};

void foo(int s)
{
  Vector junk(s);
}

int main()
{
  Vector* test;
  for (int i=0;i<40;i++) // was 100000
        foo(1000);
  printf ("PASS\n");
}
