// PR c++/98620
// { dg-do compile { target c++11 } }

namespace std {
  template<typename T>
  T&& declval() noexcept;

  template<bool B>
  struct bool_constant {
    static constexpr bool value = B;
    using type = bool_constant;
  };
  using true_type = bool_constant<true>;
  using false_type = bool_constant<false>;
};

template <typename T>
struct TmpArray
{
   T arr[1];
};

template <typename Src, typename Dst, typename = void>
struct is_non_narrowing_conversion : std::false_type
{};

template <typename Src, typename Dst>
struct is_non_narrowing_conversion<
    Src, Dst,
    decltype(void(TmpArray<Dst>{{ std::declval<Src>() }})) // { dg-bogus "missing initializer" }
> : std::true_type
{};

struct mystruct
{
    int a;
    void * b;
};

void test_nok()
{
  is_non_narrowing_conversion<int&, mystruct>::type v;
  (void) v;
}
