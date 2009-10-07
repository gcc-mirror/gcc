// { dg-options -std=c++0x }

template<typename T>
struct NonCopyable {
  NonCopyable() = default;
  NonCopyable(NonCopyable const&);
};

template<>
NonCopyable<int>::NonCopyable(NonCopyable<int> const&) = delete; // { dg-error "deleted" }

template<typename T>
NonCopyable<T>::NonCopyable(NonCopyable<T> const&) = default;

template<>
NonCopyable<double>::NonCopyable(NonCopyable<double> const&) = delete; // { dg-error "deleted" }


int main()
{
  NonCopyable<double> nc_dbl;
  NonCopyable<double> nc_dbl_cpy(nc_dbl); // { dg-error "used here" }

  NonCopyable<int> nc_int;
  NonCopyable<int> nc_int_cpy(nc_int); // { dg-error "used here" }

  NonCopyable<char> nc_char;
  NonCopyable<char> nc_char_cpy(nc_char);
}
