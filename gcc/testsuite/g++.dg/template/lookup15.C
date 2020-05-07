// PR c++/94799 - member template function lookup fails.

template<typename>
struct M { void fn() { } };

M<int>* bar (int);
M<int> bar2 (int);

template<typename T>
struct X : M<T> {
  void xfn ()
  {
    this->template M<T>::fn ();
    bar((T)1)->template M<T>::fn ();
    bar2((T)1).template M<T>::fn ();
  }
};

int
main ()
{
  X<int> x;
  x.xfn();
}
