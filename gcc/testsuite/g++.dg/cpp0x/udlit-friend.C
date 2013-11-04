// { dg-options -std=c++11 }

long double
operator"" _Hertz(long double);

class Foo
{
public:
  Foo() { }

  friend Foo operator"" _Bar(char);

  friend long double
  operator"" _Hertz(long double omega)
  { return omega / 6.28318530717958648; }
};

Foo
operator"" _Bar(char)
{ return Foo(); }

Foo f1 = operator"" _Bar('x');

Foo f2 = 'x'_Bar;

long double fm1 = operator"" _Hertz(552.92L);

long double fm2 = 552.92_Hertz;
