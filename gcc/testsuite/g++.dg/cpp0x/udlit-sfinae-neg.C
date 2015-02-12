// { dg-do compile { target c++11 } }
// { dg-prune-output "note:" }

template<bool, typename _Tp = void> struct enable_if { };
template<typename _Tp> struct enable_if<true, _Tp> { typedef _Tp type; };

template <char... c>
constexpr typename enable_if<sizeof...(c) == 2, int>::type operator""_t () // { dg-error "no type named|in" }
{
  return 2;
}

template <char... c>
constexpr typename enable_if<sizeof...(c) == 1, int>::type operator""_t () // { dg-error "no type named|in" }
{
  return 1;
}

int a = 45_t;
int b = 4_t;
int c = 100000_t; // { dg-error "no matching function for call to" }
