// { dg-options "-std=c++1z -fconcepts" }

template <class T> concept bool is_int = __is_same_as(T,int);

template <class T> struct A { };
template <is_int T> struct A<T*> {
  typedef int I1;
  static const A<T*>::I1 j1 = 0;
  static int f();
};
template <is_int T> int A<T*>::f()
{ A<T*>::I1 i; return j1; }

template <class T> struct A<T*> {
  typedef int I2;
  static const A<T*>::I2 j2 = 0;
  static int f();
};
template <class T> int A<T*>::f()
{ A<T*>::I2 i; return j2; }

const int i1 = A<int*>::j1;
const int i2 = A<float*>::j2;

template <class T> struct B;

template <is_int T> struct B<T> {
  typedef int I4;
  static const B<T>::I4 j4 = 0;
  static int f();
};
template <is_int T> int B<T>::f()
{ B<T>::I4 i; return j4; }

template <class T> struct B {
  typedef int I5;
  static const B<T>::I5 j5 = 0;
  static int f();
};
template <class T> int B<T>::f()
{ B<T>::I5 i; return j5; }

int i4 = B<int>::j4;
int i5 = B<float>::j5;
int main()
{
  B<int>::f();
  B<float>::f();
}
