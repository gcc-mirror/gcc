// { dg-do assemble  }
// GROUPS passed templates destructors
// Example of PR 3308 workaround

template <class T>
class A
{
  T q;
public:
  ~A() { (&q)->T::~T(); }
};

typedef unsigned int ui;

int main()
{
  A<ui> *ap = new A<ui>;

  delete ap;
}
