// { dg-do compile { target c++11 } }
// { dg-options "--embed-dir=${srcdir}/c-c++-common/cpp/embed-dir" }

template <int... N>
constexpr decltype (sizeof 0)
foo ()
{
  return sizeof... (N);
}

template <typename T, typename U>
struct same_type;
template <typename T>
struct same_type <T, T> {};

void
bar ()
{
}

template <typename U, typename... T>
same_type <U, int> *
bar (U u, T... t)
{
  bar (t...);
  return nullptr;
}

static_assert (
#embed <magna-carta.txt> limit (1) prefix (foo <) suffix (> () == 1, "")
);
static_assert (
#embed <magna-carta.txt> limit (2) prefix (foo <) suffix (> () == 2, "")
);
static_assert (
#embed <magna-carta.txt> limit (42) prefix (foo <) suffix (> () == 42, "")
);
static_assert (
#embed <magna-carta.txt> limit (521) prefix (foo <) suffix (> () == 521, "")
);

void
baz ()
{
  bar (
#embed "magna-carta.txt" limit (54)
      );
}
