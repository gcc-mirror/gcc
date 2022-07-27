// { dg-do run { target { *-*-linux* && property_1_needed } } }
// { dg-options "-O2 -mno-direct-extern-access" }

class Foo 
{
public:
  Foo(int n) : n_(n) { }
  int f() { return n_; }

  int badTest();
  int goodTest();

private:

  int n_;
};

int Foo::badTest()
{
  try {
      throw int(99);
  }

  catch (int &i) {
      n_ = 16;
  }

  return n_;
}


int Foo::goodTest()
{
  int	n;

  try {
      throw int(99);
  }

  catch (int &i) {
      n = 16;
  }

  return n_;
}

int main() 
{
  Foo foo(5);
  foo.goodTest();
  foo.badTest();
  return 0;
}
