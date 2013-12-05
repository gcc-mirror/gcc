// PR c++/59044

template <class T>
class C {
private:
  template <T a, T b>
  struct Implementation {};
public:
  typedef typename Implementation<0, 0>::Typedef Type;
};

template <class T>
template <T b>
struct C<T>::Implementation<0, b> { typedef void Typedef; };

template class C<unsigned>;
