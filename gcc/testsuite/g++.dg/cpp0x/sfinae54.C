// PR c++/63723
// { dg-do compile { target c++11 } }

template<typename Tp> Tp declval();

template<typename Tp, Tp v>
struct integral_constant
{
  static constexpr Tp value = v;
  typedef Tp value_type;
  typedef integral_constant<Tp, v> type;
  constexpr operator value_type() const { return value; }
};

typedef integral_constant<bool, true>   true_type;
typedef integral_constant<bool, false> false_type;

template <typename From, typename To>
class is_list_convertible_helper
{
  template <typename To2>
  static void requires_conversion(To2 t);

  template <typename From2, typename To2,
      typename = decltype(requires_conversion<To2>({declval<From2>()}))>
  static true_type helper(int);

  template <typename From2, typename To2>
  static false_type helper(...);

public:
  using type = decltype(helper<From, To>(0));
};

template <typename From, typename To>
class is_list_convertible
  : public is_list_convertible_helper<From, To>::type
{ };

static_assert(!is_list_convertible<double, int>::value,
	      "double -> int is narrowing!");
