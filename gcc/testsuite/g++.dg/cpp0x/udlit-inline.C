// { dg-options "-std=c++0x" }

//  Literal operators can be inline.

inline int
operator"" _thing1(char cc)
{ return 42 * cc; }

int operator"" _thing2(char cc);

class Foo
{
  int
  friend operator"" _thing2(char cc)
  { return 42 * cc; }
};

int i = operator"" _thing1('x');
int j = 'x'_thing1;

int iF = operator"" _thing2('x');
int jF = 'x'_thing2;
