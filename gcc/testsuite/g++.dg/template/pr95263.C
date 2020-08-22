// { dg-do compile { target c++11 } }
// PR C++/95263
// ICE on alias template instantiation

template <typename> class TPL {
  template <int> using INT = int;
};

template <typename T> class Klass
{
public:
  template <int I> using ALIAS = typename TPL<T>::INT<I>;

  template <int> static void FUNC (); // OK

  template <int I, typename> static ALIAS<I> FUNC (); // SFINAE ICE
};

void Fn ()
{
  Klass<int>::FUNC<0> ();
}

