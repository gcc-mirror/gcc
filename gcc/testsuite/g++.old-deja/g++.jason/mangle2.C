// { dg-do assemble  }
// I guess this was broken once.

template <class C, int D> class X { };
typedef X<int, 0> T;

class Y
{
  public:
    ~Y();
};

class Z
{
  public:
    void f(T**);
};

void Z::f(T** t)
{ }

Y::~Y()
{ }
