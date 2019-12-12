// { dg-lto-do assemble }
// { dg-lto-options { { -flto -ffat-lto-objects -fdebug-types-section -g -std=gnu++17 } } }

template<typename _Tp, _Tp __v>
struct integral_constant
{
  static constexpr _Tp value = __v;
  typedef _Tp value_type;
  constexpr operator value_type() const noexcept { return value; }
};

typedef integral_constant<bool, false> false_type;

template<typename...>
struct __or_;

template<>
struct __or_<>
  : public false_type
{ };
