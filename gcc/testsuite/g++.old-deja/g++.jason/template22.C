// Testcase for proper unification of code involving references.
// Build don't link:

template<class T>
struct A
{
    void foo();
};

template<class T> void A<T>::foo() { }

template class A<int&>;

const int& f1 ();
int& f2 ();
int f3 ();

template <class T> void g1 (const T&);
template <class T> void g2 (T&);
template <class T> void g3 (T);

int main()
{
  g1 (f1 ());
  g1 (f2 ());
  g1 (f3 ());
  g2 (f2 ());
  g3 (f1 ());
  g3 (f2 ());
  g3 (f3 ());
}
