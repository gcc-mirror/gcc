// { dg-do assemble  }
// Exhaustive test for destructors of simple types.
// PRMS Id: 2744, 3308

template <class T> class A {
  T q;
public:
  ~A() {
    q.T::~T();
    q.~T();
    (&q)->T::~T();
    (&q)->~T();
  }
};

typedef char * cp;
typedef int I;

int main ()
{
  A<int> a;
  A<cp> b;
  int i;
  cp c;

  i.~I();
  i.I::~I();
  (&i)->~I();
  (&i)->I::~I();
  c.~cp();
  c.cp::~cp();
  (&c)->~cp();
  (&c)->cp::~cp();
}
