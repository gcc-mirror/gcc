//Check instantiation of templates outside their namespace
namespace A{
template <class T>void g(){}
template <class T> struct B {
  B(){
   f();
  }
  void f()
  {
    g<T>();
  }
};
}

template class A::B<int>;
A::B<int> s;

int main()
{
  return 0;
}
