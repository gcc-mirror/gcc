// Origin: PR c++/55663
// { dg-do compile { target c++11 } }

template <typename>
constexpr bool the_truth () { return true; }

template <bool>
  struct Takes_bool { };

template<bool B>
  using Alias = Takes_bool<B>;

template<typename T>
  struct test { using type = Alias<the_truth<T>()>; };

int main () {
  test<int> a;

  return 0;
}
