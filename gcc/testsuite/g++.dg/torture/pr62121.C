// { dg-do compile }
class A
{
  virtual double operator()();
};
class B : A
{
public:
  double operator()();
};
extern B a[];
int b = a[0]();
