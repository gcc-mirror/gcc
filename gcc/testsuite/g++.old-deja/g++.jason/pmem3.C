// { dg-do run  }
// Test that comparison of pointers to members does not complain about
// contravariance violation.

struct A { int i; };
struct B : public A { int j; int f (); };
int main ()
{
  int A::*apm = &A::i;
  int B::*bpm = apm;
  return apm != bpm;
}
