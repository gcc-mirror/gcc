// Build don't link:
// Origin: Mathias Doreille <Mathias.Doreille@imag.fr>

template<class T>
struct  a {
  struct b {
    T operator()();
  };
};


template<class T>
T a<T>::b::operator()() { return T(0); }

template<> int a<int>::b::operator()() { return 1; }
