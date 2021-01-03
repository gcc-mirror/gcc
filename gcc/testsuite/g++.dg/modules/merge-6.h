
template<bool>
struct __truth_type;

template<typename T>
struct __traitor
{
  enum { __value = true }; // Oh, an anonymous templatey thing!
  typedef typename __truth_type<__value>::__type __type;
};
