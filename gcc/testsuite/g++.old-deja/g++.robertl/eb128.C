template<class T>
struct A {
  typedef T* iterator;
public:
  A(){}
};

void f()
{
  A<int&> a;  // ERROR - pointer to reference
}
