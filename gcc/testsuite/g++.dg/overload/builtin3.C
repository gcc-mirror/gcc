// PR c++/11409
// { dg-do compile }

namespace std {
  double fabs (double);
}
using std::fabs;

double (*p) (double) = &fabs;  // { dg-bogus "is ambiguous" }

