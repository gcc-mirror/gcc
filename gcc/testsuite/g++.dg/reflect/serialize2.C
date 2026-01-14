// { dg-do run { target c++26 } }
// { dg-additional-options "-freflection -O0" }

#include <meta>
#include <cstdlib>

namespace impl {
  template <auto... vals>
  struct replicator_type {
    template <typename F>
    constexpr void operator >> (F body) const
    { (body.template operator()<vals>(), ...); }
  };

  template <auto... vals>
  replicator_type <vals...> replicator = {};
}

template <typename R>
consteval auto
expand (R range)
{
  std::vector <std::meta::info> args;
  for (auto r : range)
    args.push_back (std::meta::reflect_constant (r));
  return substitute (^^impl::replicator, args);
}

struct Person {
  std::string name;
  int age;
};

template <typename S>
constexpr std::string
serialize (S s)
{
  std::string result = " ";
#if 0
  // TODO, this ICEs in tsubst_expr.
  impl::replicator <^^S::age, ^^S::name> >> [&] <auto m> {
#else
  impl::replicator <^^Person::age, ^^Person::name> >> [&] <auto m> {
#endif
    result += identifier_of (m);
    result += "=";
    if constexpr (type_of (m) == ^^int)
      result += std::string (s.[: m :] / 10, 'X');
    else
      result += s.[: m :];
    result += " ";
  };
  return result;
}

constexpr Person john { "John", 42 };

static_assert (serialize (john) == " age=XXXX name=John ");

int
main ()
{
  Person jack { "Jack", 53 };
  std::string s = serialize (jack);
  if (s != " age=XXXXX name=Jack ")
    std::abort ();
}
