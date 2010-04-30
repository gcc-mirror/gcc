// PR c++/43890
// { dg-do compile }

class Outer
{
  public:
  Outer()
  : i(*this)
  {
  }

  class Inner
  {
    public:
    Inner(Outer& o)
    : o(o)
    , i(0)
    {
    }

    private:
    Outer& o;
    int const i;
  };

  private:
  Inner i;
};

class A {
  Outer o;
};

int main()
{
  A *a = new A;

  return 0;
}
