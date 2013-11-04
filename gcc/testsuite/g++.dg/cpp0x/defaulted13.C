// { dg-options -std=c++11 }

template<typename T>
struct NonCopyable {
  NonCopyable() = default;
  NonCopyable(NonCopyable const&);
};

template<>
NonCopyable<int>::NonCopyable(NonCopyable<int> const&) = delete; // { dg-message "declared" }

template<typename T>
NonCopyable<T>::NonCopyable(NonCopyable<T> const&) = default;

template<>
NonCopyable<double>::NonCopyable(NonCopyable<double> const&) = delete; // { dg-message "declared" }


int main()
{
  NonCopyable<double> nc_dbl;
  NonCopyable<double> nc_dbl_cpy(nc_dbl); // { dg-error "use" }

  NonCopyable<int> nc_int;
  NonCopyable<int> nc_int_cpy(nc_int); // { dg-error "use" }

  NonCopyable<char> nc_char;
  NonCopyable<char> nc_char_cpy(nc_char);
}
