// PR c++/60517 - warning/error for taking address of member of a temporary
// object
// { dg-do compile }

class B
{
public:
  double x[2];
};

class A
{
  B b;
public:
  B getB () { return b; }
};

double foo (A a)
{
  double * x = &(a.getB().x[0]);   // { dg-error "taking address of rvalue" }
  return x[0];
}
