namespace A
{
  typedef int T;
}

class A::T x; // { dg-error "" }
