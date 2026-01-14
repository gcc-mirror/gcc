// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::operator_of.

#include <meta>

using namespace std::meta;

constexpr info null_reflection;
struct cls {
  int dm;
  static int static_dm;
  void mem_fun ();
  static void static_mem_fun ();
  int &ref_dm = dm;
  using type = int;
} cls_var;
union onion { };
static union { int anon; };
using alias = cls;
void fun ();
int var;
int &ref = var;
int &&rref = 42;
int *ptr = &var;
namespace ns {}
namespace ns_alias = ns;
enum Enum { A };
enum class Enum_class { A };

template<typename> struct incomplete_cls;
template<typename> struct cls_tmpl {};
template<typename> void fun_tmpl ();
template<typename> concept conc = requires { true; };
template<typename> int var_tmpl;
template<typename T> using cls_tmpl_alias = cls_tmpl<T>;

int arr[] = { 42 };
auto [ decomp ] = arr;
auto &[ decomp_ref ] = arr;

consteval bool
is_operator (info r)
{
  try { operator_of (r); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (!is_operator (null_reflection));
static_assert (!is_operator (^^::));
static_assert (!is_operator (^^ns));
static_assert (!is_operator (^^ns_alias));
static_assert (!is_operator (reflect_constant (3)));
static_assert (!is_operator (^^cls));
static_assert (!is_operator (^^cls::dm));
static_assert (!is_operator (^^cls::ref_dm));
static_assert (!is_operator (^^cls::static_dm));
static_assert (!is_operator (^^cls::mem_fun));
static_assert (!is_operator (^^cls::static_mem_fun));
static_assert (!is_operator (^^cls::type));
static_assert (!is_operator (^^cls_var));
static_assert (!is_operator (^^onion));
static_assert (!is_operator (^^anon));
static_assert (!is_operator (^^fun));
static_assert (!is_operator (^^alias));
static_assert (!is_operator (^^var));
static_assert (!is_operator (^^ref));
static_assert (!is_operator (^^rref));
static_assert (!is_operator (^^ptr));
static_assert (!is_operator (^^cls_tmpl));
static_assert (!is_operator (^^cls_tmpl<int>));
static_assert (!is_operator (^^incomplete_cls<int>));
static_assert (!is_operator (^^fun_tmpl));
static_assert (!is_operator (^^fun_tmpl<int>));
static_assert (!is_operator (^^conc));
static_assert (!is_operator (substitute (^^conc, { ^^int })));
static_assert (!is_operator (^^var_tmpl));
static_assert (!is_operator (^^var_tmpl<int>));
static_assert (!is_operator (^^cls_tmpl_alias));
static_assert (!is_operator (^^cls_tmpl_alias<int>));
static_assert (!is_operator (^^Enum));
static_assert (!is_operator (^^Enum::A));
static_assert (!is_operator (^^Enum_class));
static_assert (!is_operator (^^Enum_class::A));
static_assert (!is_operator (^^decomp));
static_assert (!is_operator (^^decomp_ref));
static_assert (!is_operator (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_operator (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_operator (bases_of (^^Derived, access_context::current ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!is_operator (^^T));
  static_assert (!is_operator (R));
  static_assert (!is_operator (R2));
  static_assert (!is_operator (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^cls>();
  static_assert (!is_operator (^^p));
  static_assert (!is_operator (^^c));
}

struct S
{
  using size_t = decltype (sizeof 0);
  void *operator new (size_t, void *);
  void *operator new[] (size_t, void *);
  void operator delete (void *);
  void operator delete[] (void *);
  S &operator () (const S &);
  S &operator [] (const S &);
  S &operator = (const S &);
  S &operator << (int);
  S &operator >> (int);
  S &operator ++ ();
  S &operator -- ();
  S &operator ~ ();
  S &operator ! ();
  S &operator + (const S &);
  S &operator - (const S &);
  S &operator * (const S &);
  S &operator / (const S &);
  S &operator % (const S &);
  S &operator ^ (const S &);
  S &operator & (const S &);
  S &operator | (const S &);
  S &operator += (const S &);
  S &operator -= (const S &);
  S &operator *= (const S &);
  S &operator /= (const S &);
  S &operator %= (const S &);
  S &operator ^= (const S &);
  S &operator &= (const S &);
  S &operator |= (const S &);
  S &operator <<= (int);
  S &operator >>= (int);
  bool operator == (const S &);
  bool operator != (const S &);
  bool operator < (const S &);
  bool operator > (const S &);
  bool operator <= (const S &);
  bool operator >= (const S &);
  int operator <=> (const S &);
  bool operator && (const S &);
  bool operator || (const S &);
  S &operator , (const S &);
  S *operator -> ();
  S &operator ->* (const S &);
  S &operator co_await ();
};

struct T
{
  T &operator ++ (int);
  T &operator -- (int);	
  T &operator + ();
  T &operator - ();
};

struct U
{
  U &operator compl ();
  U &operator not ();
  U &operator xor (const U &);
  U &operator bitand (const U &);
  U &operator bitor (const U &);
  U &operator xor_eq (const U &);
  U &operator and_eq (const U &);
  U &operator or_eq (const U &);
  bool operator and (const U &);
  bool operator or (const U &);

  template <typename... Args>
  S &operator () (const Args&...);
  template <typename... Args>
  S &operator [] (const Args&...);
};

struct W
{
  using size_t = decltype (sizeof 0);
  template <int> void *operator new (size_t, void *);
  template <int> void *operator new[] (size_t, void *);
  template <int> void operator delete (void *, W);
  template <int> void operator delete[] (void *, W);
  template <int> W &operator () (const W &);
  template <int> W &operator [] (const W &);
  template <int> W &operator = (const W &);
  template <int> W &operator << (int);
  template <int> W &operator >> (int);
  template <int> W &operator ++ ();
  template <int> W &operator -- ();
  template <int> W &operator ~ ();
  template <int> W &operator ! ();
  template <int> W &operator + (const W &);
  template <int> W &operator - (const W &);
  template <int> W &operator * (const W &);
  template <int> W &operator / (const W &);
  template <int> W &operator % (const W &);
  template <int> W &operator ^ (const W &);
  template <int> W &operator & (const W &);
  template <int> W &operator | (const W &);
  template <int> W &operator += (const W &);
  template <int> W &operator -= (const W &);
  template <int> W &operator *= (const W &);
  template <int> W &operator /= (const W &);
  template <int> W &operator %= (const W &);
  template <int> W &operator ^= (const W &);
  template <int> W &operator &= (const W &);
  template <int> W &operator |= (const W &);
  template <int> W &operator <<= (int);
  template <int> W &operator >>= (int);
  template <int> bool operator == (const W &);
  template <int> bool operator != (const W &);
  template <int> bool operator < (const W &);
  template <int> bool operator > (const W &);
  template <int> bool operator <= (const W &);
  template <int> bool operator >= (const W &);
  template <int> int operator <=> (const W &);
  template <int> bool operator && (const W &);
  template <int> bool operator || (const W &);
  template <int> W &operator , (const W &);
  template <int> W *operator -> ();
  template <int> W &operator ->* (const W &);
  template <int> W &operator co_await ();
};

struct X
{
  template <int> X &operator ++ (int);
  template <int> X &operator -- (int);	
  template <int> X &operator + ();
  template <int> X &operator - ();
};

static_assert (operator_of (^^S::operator new) == op_new);
static_assert (operator_of (^^S::operator delete) == std::meta::op_delete);
static_assert (operator_of (^^S::operator new[]) == std::meta::operators::op_array_new);
static_assert (operator_of (^^S::operator delete[]) == op_array_delete);
static_assert (operator_of (^^S::operator co_await) == op_co_await);
static_assert (operator_of (^^S::operator ()) == op_parentheses);
static_assert (operator_of (^^S::operator []) == op_square_brackets);
static_assert (operator_of (^^S::operator ->) == op_arrow);
static_assert (operator_of (^^S::operator ->*) == op_arrow_star);
static_assert (operator_of (^^S::operator ~) == op_tilde);
static_assert (operator_of (^^S::operator !) == op_exclamation);
static_assert (operator_of (^^S::operator +) == op_plus);
static_assert (operator_of (^^S::operator -) == op_minus);
static_assert (operator_of (^^S::operator *) == op_star);
static_assert (operator_of (^^S::operator /) == op_slash);
static_assert (operator_of (^^S::operator %) == op_percent);
static_assert (operator_of (^^S::operator ^) == op_caret);
static_assert (operator_of (^^S::operator &) == op_ampersand);
static_assert (operator_of (^^S::operator =) == op_equals);
static_assert (operator_of (^^S::operator |) == op_pipe);
static_assert (operator_of (^^S::operator +=) == op_plus_equals);
static_assert (operator_of (^^S::operator -=) == op_minus_equals);
static_assert (operator_of (^^S::operator *=) == op_star_equals);
static_assert (operator_of (^^S::operator /=) == op_slash_equals);
static_assert (operator_of (^^S::operator %=) == op_percent_equals);
static_assert (operator_of (^^S::operator ^=) == op_caret_equals);
static_assert (operator_of (^^S::operator &=) == op_ampersand_equals);
static_assert (operator_of (^^S::operator |=) == op_pipe_equals);
static_assert (operator_of (^^S::operator ==) == op_equals_equals);
static_assert (operator_of (^^S::operator !=) == op_exclamation_equals);
static_assert (operator_of (^^S::operator <) == op_less);
static_assert (operator_of (^^S::operator >) == op_greater);
static_assert (operator_of (^^S::operator <=) == op_less_equals);
static_assert (operator_of (^^S::operator >=) == op_greater_equals);
static_assert (operator_of (^^S::operator <=>) == op_spaceship);
static_assert (operator_of (^^S::operator &&) == op_ampersand_ampersand);
static_assert (operator_of (^^S::operator ||) == op_pipe_pipe);
static_assert (operator_of (^^S::operator <<) == op_less_less);
static_assert (operator_of (^^S::operator >>) == op_greater_greater);
static_assert (operator_of (^^S::operator <<=) == op_less_less_equals);
static_assert (operator_of (^^S::operator >>=) == op_greater_greater_equals);
static_assert (operator_of (^^S::operator ++) == op_plus_plus);
static_assert (operator_of (^^S::operator --) == op_minus_minus);
static_assert (operator_of (^^S::operator ,) == op_comma);
static_assert (operator_of (^^T::operator +) == op_plus);
static_assert (operator_of (^^T::operator -) == op_minus);
static_assert (operator_of (^^T::operator ++) == op_plus_plus);
static_assert (operator_of (^^T::operator --) == op_minus_minus);
static_assert (operator_of (^^U::operator compl) == op_tilde);
static_assert (operator_of (^^U::operator not) == op_exclamation);
static_assert (operator_of (^^U::operator bitand) == op_ampersand);
static_assert (operator_of (^^U::operator bitor) == op_pipe);
static_assert (operator_of (^^U::operator xor_eq) == op_caret_equals);
static_assert (operator_of (^^U::operator and_eq) == op_ampersand_equals);
static_assert (operator_of (^^U::operator or_eq) == op_pipe_equals);
static_assert (operator_of (^^U::operator and) == op_ampersand_ampersand);
static_assert (operator_of (^^U::operator or) == op_pipe_pipe);
static_assert (operator_of (^^U::operator ()) == op_parentheses);
static_assert (operator_of (^^U::operator []) == op_square_brackets);
static_assert (operator_of (^^W::operator new) == op_new);
static_assert (operator_of (^^W::operator delete) == std::meta::op_delete);
static_assert (operator_of (^^W::operator new[]) == std::meta::operators::op_array_new);
static_assert (operator_of (^^W::operator delete[]) == op_array_delete);
static_assert (operator_of (^^W::operator co_await) == op_co_await);
static_assert (operator_of (^^W::operator ()) == op_parentheses);
static_assert (operator_of (^^W::operator []) == op_square_brackets);
static_assert (operator_of (^^W::operator ->) == op_arrow);
static_assert (operator_of (^^W::operator ->*) == op_arrow_star);
static_assert (operator_of (^^W::operator ~) == op_tilde);
static_assert (operator_of (^^W::operator !) == op_exclamation);
static_assert (operator_of (^^W::operator +) == op_plus);
static_assert (operator_of (^^W::operator -) == op_minus);
static_assert (operator_of (^^W::operator *) == op_star);
static_assert (operator_of (^^W::operator /) == op_slash);
static_assert (operator_of (^^W::operator %) == op_percent);
static_assert (operator_of (^^W::operator ^) == op_caret);
static_assert (operator_of (^^W::operator &) == op_ampersand);
static_assert (operator_of (template_of (^^W::operator = <0>)) == op_equals);
static_assert (operator_of (^^W::operator |) == op_pipe);
static_assert (operator_of (^^W::operator +=) == op_plus_equals);
static_assert (operator_of (^^W::operator -=) == op_minus_equals);
static_assert (operator_of (^^W::operator *=) == op_star_equals);
static_assert (operator_of (^^W::operator /=) == op_slash_equals);
static_assert (operator_of (^^W::operator %=) == op_percent_equals);
static_assert (operator_of (^^W::operator ^=) == op_caret_equals);
static_assert (operator_of (^^W::operator &=) == op_ampersand_equals);
static_assert (operator_of (^^W::operator |=) == op_pipe_equals);
static_assert (operator_of (^^W::operator ==) == op_equals_equals);
static_assert (operator_of (^^W::operator !=) == op_exclamation_equals);
static_assert (operator_of (^^W::operator <) == op_less);
static_assert (operator_of (^^W::operator >) == op_greater);
static_assert (operator_of (^^W::operator <=) == op_less_equals);
static_assert (operator_of (^^W::operator >=) == op_greater_equals);
static_assert (operator_of (^^W::operator <=>) == op_spaceship);
static_assert (operator_of (^^W::operator &&) == op_ampersand_ampersand);
static_assert (operator_of (^^W::operator ||) == op_pipe_pipe);
static_assert (operator_of (^^W::operator <<) == op_less_less);
static_assert (operator_of (^^W::operator >>) == op_greater_greater);
static_assert (operator_of (^^W::operator <<=) == op_less_less_equals);
static_assert (operator_of (^^W::operator >>=) == op_greater_greater_equals);
static_assert (operator_of (^^W::operator ++) == op_plus_plus);
static_assert (operator_of (^^W::operator --) == op_minus_minus);
static_assert (operator_of (^^W::operator ,) == op_comma);
static_assert (operator_of (^^X::operator +) == op_plus);
static_assert (operator_of (^^X::operator -) == op_minus);
static_assert (operator_of (^^X::operator ++) == op_plus_plus);
static_assert (operator_of (^^X::operator --) == op_minus_minus);
