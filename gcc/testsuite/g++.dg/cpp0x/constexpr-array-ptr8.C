// PR c++/57047
// { dg-require-effective-target c++11 }

template <typename>
struct A;
template <typename T>
struct A <T &>
{
  typedef T type;
};
template <typename T>
constexpr T && foo (typename A <T>::type & __t) noexcept
{
  return static_cast <T &&>(__t);
}
template <class T1, class T2>
struct B
{
  T1 t1;
  T2 t2;
  template <class U>
  constexpr B (U && __x, const T2 & __y) : t1 (foo <U> (__x)), t2 (__y) {}
};
static inline constexpr bool
fn1 (const char c)
{
  return ('0' <= c) && (c <= '9');
}
static inline constexpr bool
fn2 (const char c)
{
  return (('A' <= c) && (c <= 'Z')) || (('a' <= c) && (c <= 'z'));
}
static constexpr bool
fn3 (const char *const x)
{
  return (x[1] == '\0' && x[0] == ']') ? true : (!fn1 (x[0])) ? false : fn3 (&x[1]);
}
static constexpr bool
fn4 (const char *const x)
{
  return (x[0] == '\0') ? fn3 (&x[1]) : fn4 (&x[1]);
}
static inline constexpr bool
fn5 (const char *const x)
{
  return fn2 (x[0]) ? fn4 (x) : false;
}
struct C final
{
  constexpr C (const char *const t1) : c (fn5 (t1) ? 199 : 69) {}
  unsigned c;
};
B <C, C> p ("a", "b");
