// PR c++/65554
// { dg-do compile { target c++11 } }

typedef decltype (sizeof (int)) size_type;

namespace std
{
template <class> class initializer_list
{
  int *_M_array;
  size_type _M_len;
};
}
