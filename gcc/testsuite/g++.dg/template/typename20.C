// PR c++/56543

template <typename>
struct S;

template <typename T>
struct U
{
  typedef typename S <T>::template V <> W;
  S <W> x;
};
