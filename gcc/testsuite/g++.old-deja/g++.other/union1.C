// { dg-do assemble  }

class A
{
  private:
    int myInt;

  public:
    A& operator = (int right) {myInt = right; return *this;}
};

union B
{
    char f1;
    A    f2;   // { dg-bogus "" } non-copy assignment op is OK
};
