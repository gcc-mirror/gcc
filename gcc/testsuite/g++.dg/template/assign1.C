// PR c++/16623

template <int N>
struct C
{
  C& operator= (int);
};

template <int N>
C<N>& C<N>::operator= (int)
{
  return *this;
}

C<0> a;
