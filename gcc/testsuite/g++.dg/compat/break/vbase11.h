struct base
{
  short b;
  virtual int foo();
};

struct derived: virtual base
{
  int d;
  virtual int foo();
  virtual int bar();
};
