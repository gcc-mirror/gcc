template<class T>
struct A {
  typedef T* iterator; // ERROR - pointer to reference
public:
  A(){}
};

void f()
{
  A<int&> a; // ERROR - instantiated from here
}
