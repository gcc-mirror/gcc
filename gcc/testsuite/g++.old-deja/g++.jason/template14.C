// { dg-do run  }
// PRMS Id: 4745
// Bug: g++ gets the constructor and destructor confused because the default
// parm prevents the two constructor types from satisfying ==.

template <class T> struct A {
  A(int = 1);
  ~A();
};

template <class T> A<T>::A(int) { } // causes compiler abort
template <class T> A<T>::~A() { }

int main()
{
  A<int> a;
}
