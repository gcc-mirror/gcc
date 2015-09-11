// PR c++/65554
// { dg-do compile { target c++11 } }

namespace std
{
template <class> class initializer_list // { dg-error "definition of std::initializer_list does not match" }
{
  int *_M_array;
  int _M_len;
};
}

// { dg-prune-output "compilation terminated" }
