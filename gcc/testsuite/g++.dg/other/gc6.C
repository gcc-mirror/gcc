// PR c++/99831
// { dg-do compile { target c++20 } }
// { dg-options "--param ggc-min-heapsize=0 --param ggc-min-expand=0" }

template <int N> struct S {
  constexpr S(const char (&str)[N]) : value{} { }
  char value[N];
};
template <S> struct string {
  constexpr bool operator==(const string &) const = default;
};
template <S L2> void operator+(string<L2>) {
  char value[1];
  S{value};
}
static_assert(string<"a">{} == string<"a">{});
