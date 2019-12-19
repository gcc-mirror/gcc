
template<bool> struct __truth_type;

template<typename T> struct __traitor
{
  enum X { __value = true };
  typedef typename __truth_type<__value>::__type __type;
};
