// PR c++/51045
// { dg-options "-Wzero-as-null-pointer-constant" }

template <typename T>
struct A
{
  A() { t = new T; }

  ~A()
  {
    delete t;
  }
  T* t;
};

template <typename T>
struct B
{
  B() { t = new T[1]; }

  ~B()
  {
    delete [] t;
  }
  T* t;
};

template <typename Type>
class Variant
{
  Type t;
};

class Op;

typedef Variant<A<Op> > vara;
typedef Variant<B<Op> > varb;

class Op
{
  vara x;
  varb y;
};

int main()
{
  vara a;
  varb b;
}
