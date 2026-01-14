// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Slightly tweaked test from P3394R4 3.2
// https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2025/p3394r4.html#test-parametrization
// TODO: Doesn't link currently, once it does, it should be dg-do run test
// with output checking or something like that.

#include <meta>
#include <array>
#include <print>

namespace impl {
  template <auto... vals>
  struct replicator_type {
    template <typename F>
    constexpr void operator >> (F body) const
    {
      (body.template operator () <vals> (), ...);
    }
  };

  template <auto... vals>
  replicator_type <vals...> replicator = {};
}

template <typename R>
consteval auto
expand (R range)
{
  std::vector<std::meta::info> args;
  for (auto r : range)
    args.push_back (std::meta::reflect_constant (r));
  return substitute (^^impl::replicator, args);
}

template <size_t N, class F>
constexpr decltype (auto)
with_indices (F f)
{
  return [&] <size_t... Is> (std::index_sequence <Is...>) -> decltype (auto) {
	   return f(std::integral_constant<size_t, Is>{}...);
	 }  (std::make_index_sequence <N> {});
}

template <typename... Ts>
struct Tuple {
  struct storage;
  consteval {
    define_aggregate (^^storage, { data_member_spec (^^Ts, { .name = "_" })... });
  }
  static constexpr auto ctx = std::meta::access_context::current ();
  static constexpr auto nsdms
    = define_static_array (nonstatic_data_members_of (^^storage, ctx));

  storage s;

  template <class F>
  decltype (auto) apply (F f)
  {
    return with_indices <sizeof... (Ts)> ([&] (auto... Is) -> decltype (auto) {
					    return f (s.[: nsdms[Is] :]...);
					  });
  }
};

template <typename... Ts>
Tuple (Ts...) -> Tuple <Ts...>;

template <size_t N, typename... Ts>
struct Parametrize {
  Tuple <Ts...> arr[N];
  auto begin () const { return arr; }
  auto end () const { return arr + N; }
};

template <typename... Ts, size_t N>
constexpr auto
parametrize (Tuple<Ts...> (&&arr) [N])
{
  return with_indices <N> ([&] (auto... Is) {
			     return Parametrize <N, Ts...> { { arr[Is]... } };
			   });
}

consteval auto
nonstatic_member_functions_of (std::meta::info type)
{
  auto ctx = std::meta::access_context::current ();
  auto members = members_of (type, ctx);
  std::erase_if (members, [] (std::meta::info m) {
			    return not (is_function (m)
					and not is_static_member (m)
					and not is_special_member_function (m));
			  });
  return members;
}

consteval std::meta::info
parametrization_of (std::meta::info M)
{
  for (auto a : annotations_of (M))
    {
      auto t = type_of (a);
      if (has_template_arguments (t) and template_of (t) == ^^Parametrize)
	return a;
    }
  return std::meta::info ();
}

template <std::meta::info M, class F>
void
invoke_single_test (F f)
{
  constexpr auto A = parametrization_of (M);

  if constexpr (A != std::meta::info ())
    {
      constexpr auto Params = extract <typename [: type_of (A) :]> (A);
      for (auto P : Params)
	P.apply (f);
    }
  else
    f ();
}

template <std::meta::info M>
void
invoke_single_test ()
{
  invoke_single_test <M> ([: M :]);
}

template <std::meta::info Namespace>
void
invoke_all ()
{
  [: expand (members_of (Namespace, std::meta::access_context::current ())) :]
    >> [] <std::meta::info M> {
	 if constexpr (is_function(M) and identifier_of (M).starts_with ("test_"))
	   invoke_single_test <M> ();
	 else if constexpr (is_type (M))
	   [: expand (nonstatic_member_functions_of (M)) :]
	     >> [] <std::meta::info F> {
		  if constexpr (identifier_of (F).starts_with ("test_"))
		    invoke_single_test <F> ([&] (auto... args) {
					      typename [: M :] fixture;
					      fixture.[: F :] (args...);
					    });
		};
       };
}

namespace N
{
  [[=parametrize ({ Tuple {1, 1, 2}, Tuple {1, 2, 3} })]]
  void test_sum (int x, int y, int z)
  {
    std::println ("Called test_sum (x={}, y={}, z={})", x, y, z);
  }

  struct Fixture {
    Fixture () { std::println ("setup fixture"); }
    ~Fixture() { std::println ("teardown fixture"); }
    [[=parametrize ({ Tuple {1}, Tuple {2} })]]
    void test_one (int x) { std::println ("test one({})", x); }
    void test_two () { std::println("test two"); }
  };
}

int
main ()
{
  invoke_all <^^N> ();
}
