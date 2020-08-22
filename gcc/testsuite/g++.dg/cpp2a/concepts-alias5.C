// PR c++/93377
// { dg-do compile { target c++20 } }

struct empty
{};

template <typename c>
c value;

template <typename c>
auto func(value<c>);

template <typename, typename...>
struct alignment_algorithm;

template <typename... args_t>
struct select
{
  template <typename algorithm_t, typename... _args_t>
  decltype(algorithm_t()(func<_args_t>...)) choose();

  template <typename...>
  static empty choose();

  using type = decltype(choose<alignment_algorithm<int>, args_t...>());
};

template <typename, typename... args_t>
struct select_algorithm : select<args_t...>
{};

template <typename, typename = void> struct maybe_value { int value; };

template <typename cn>
struct maybe_value<cn, typename cn::sfinae>;

struct function
{
  template <typename algorithm_t,
            typename = decltype(
                maybe_value<select_algorithm<algorithm_t, int>>::value)>
  function(algorithm_t);
};

template <typename>
struct alignment_configuration_traits
{
  static constexpr bool is_vectorised = 0;
};

template <typename config_t, typename...>
struct alignment_algorithm
{
  using traits_t = alignment_configuration_traits<config_t>;
  template <typename indexed_sequence_pairs_t>
  void operator()(indexed_sequence_pairs_t) requires traits_t::is_vectorised;
};

int main()
{
    function{alignment_algorithm<int>{}};
}
