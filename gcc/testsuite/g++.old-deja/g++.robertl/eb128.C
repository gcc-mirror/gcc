// { dg-do assemble  }
template<class T>
struct A {
  typedef T* iterator; // { dg-error "" } pointer to reference
public:
  A(){}
};

void f()
{
  A<int&> a;
}
