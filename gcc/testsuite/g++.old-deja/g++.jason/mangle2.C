// I guess this was broken once.
// Build don't link:

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
