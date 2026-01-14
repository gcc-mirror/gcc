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

struct Person1 {
  std::string name;
  int age;
};

constexpr std::string
serialize1 (Person1 s)
{
  std::string result = " ";
  impl::replicator <^^Person1::name, ^^Person1::age> >> [&] <auto m> {
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

struct Person2 {
  int age;
  std::string name;
};

constexpr std::string
serialize2 (Person2 s)
{
  std::string result = " ";
  constexpr auto ctx = std::meta::access_context::current ();
  [: expand (nonstatic_data_members_of (^^Person2, ctx)) :] >> [&] <auto m> {
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

struct Person3 {
  std::string name, surname;
  int age;
};

template <typename S>
constexpr std::string
serialize3 (S s)
{
  std::string result = " ";
  constexpr auto ctx = std::meta::access_context::current ();
  [: expand (nonstatic_data_members_of (^^S, ctx)) :] >> [&] <auto m> {
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

template <typename S>
constexpr std::string
serialize4 (S s)
{
  std::string result = " ";
  constexpr auto ctx = std::meta::access_context::current ();
  template for (constexpr auto m : [: reflect_constant_array (nonstatic_data_members_of (^^S, ctx)) :]) {
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

constexpr Person1 john { "John", 42 };
constexpr Person2 joe { 37, "Joe" };
constexpr Person3 john_smith { "John", "Smith", 71 };

static_assert (serialize1 (john) == " name=John age=XXXX ");
static_assert (serialize2 (joe) == " age=XXX name=Joe ");
static_assert (serialize3 (john) == " name=John age=XXXX ");
static_assert (serialize3 (joe) == " age=XXX name=Joe ");
static_assert (serialize3 (john_smith) == " name=John surname=Smith age=XXXXXXX ");
static_assert (serialize4 (john) == " name=John age=XXXX ");
static_assert (serialize4 (joe) == " age=XXX name=Joe ");
static_assert (serialize4 (john_smith) == " name=John surname=Smith age=XXXXXXX ");

int
main ()
{
  Person1 jack { "Jack", 53 };
  std::string s1 = serialize1 (jack);
  if (s1 != " name=Jack age=XXXXX ")
    std::abort ();
  Person2 harry { 28, "Harry" };
  std::string s2 = serialize2 (harry);
  if (s2 != " age=XX name=Harry ")
    std::abort ();
  if (serialize3 (jack) != s1)
    std::abort ();
  if (serialize3 (harry) != s2)
    std::abort ();
  Person3 jane_doe { "Jane", "Doe", 19 };
  std::string s3 = serialize3 (jane_doe);
  if (s3 != " name=Jane surname=Doe age=X ")
    std::abort ();
  if (serialize4 (jack) != s1)
    std::abort ();
  if (serialize4 (harry) != s2)
    std::abort ();
  if (serialize4 (jane_doe) != s3)
    std::abort ();
}
