// Exhaustive test for destructors of simple types.
// PRMS Id: 2744, 3308
// Build don't link:

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

int main ()
{
  A<int> a;
  A<cp> b;
  int i;
  cp c;

  i.~int();
  i.int::~int();
  (&i)->~int();
  (&i)->int::~int();
  c.~cp();
  c.cp::~cp();
  (&c)->~cp();
  (&c)->cp::~cp();
}
