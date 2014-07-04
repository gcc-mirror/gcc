// PR middle-end/61654
// { dg-do compile }

class A
{
  virtual int a (int, int = 0) = 0;
  int b (const int &);
  int c;
};

class B : virtual A
{
  int d;
  int a (int, int);
};

int
A::b (const int &)
{
  return a ('\0');
}

int
B::a (int, int)
{
  return 0 ? 0 : d;
}
