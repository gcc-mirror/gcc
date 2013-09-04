// PR c++/58119

template <class T>
struct A
{
  operator T*();
  template <class U>
  operator A<U>();
};

template <class T>
struct B
{
  operator T*();
  template <class U>
  operator A<U>*();
};

int main()
{
  A<int> a;
  delete a;

  B<int> b;
  delete b;			// { dg-error "template|delete" }
}
