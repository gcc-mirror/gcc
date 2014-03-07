// { dg-do compile { target c++11 } }
template<typename _Tp>
_Tp&& declval() noexcept;

template<bool b>
struct bt {
    static constexpr bool value = b;
};

template <typename To_, typename... From_>
class my_is_convertible_many {
  private:
    template <typename To>
      struct indirector {
	indirector(To);
      };

    template <typename To, typename... From>
      struct tag {};

    template <typename To, typename... From>
      static auto test(tag<To, From...>)
      -> decltype(indirector<To>({declval<From>()...}), bt<true>());
    static auto test(...)
      -> bt<false>;

  public:
    static constexpr bool value = decltype(test(tag<To_, From_...>()))::value;
};

struct A {};
struct B {};
struct C {};

struct Test {
  Test(A, A);
  //Test(B, B);
  explicit Test(C, C);
}; 

int main() {    
  static_assert(my_is_convertible_many<Test, A, A>::value,""); // true, correct
  static_assert(!my_is_convertible_many<Test, B, B>::value,""); // false, correct
  static_assert(!my_is_convertible_many<Test, C, C>::value,""); // error
  return 0;
}
