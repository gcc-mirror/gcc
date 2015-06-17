// PR c++/66515
// { dg-do compile { target c++11 } }

namespace std
{
  template <class _E> class initializer_list
  {
    const _E *_M_array;
    unsigned long _M_len;
  };
}

struct type_t { };

type_t &
get ()
{
  std::initializer_list<type_t>{ { get () } };
}
