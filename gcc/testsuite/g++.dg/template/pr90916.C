// PR c++/90916 ICE in retrieve_specialization

template <typename> struct S
{
  struct A;
  struct f A ();
};
template class S <int>;
