// { dg-do link }
// { dg-options "-frepo" }
// Build then link:

template <class T>
struct S {
  ~S ();
};

template <class T>
S<T>::~S () {}

int main ()
{
  S<int> s;
}
