// PR c++/23287

template <class T> struct A
{
  int i;
  ~A();
}; 

template <class T> void f(A<T> *ap) {
  ap->~A(); 
} 

template <class T> void g(A<T> *ap) {
  ap->~B(); 			// { dg-error "destructor name" }
} 

int main()
{
  f(new A<int>);
  g(new A<int>);
}
