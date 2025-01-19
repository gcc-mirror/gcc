// P2662R3 - Pack Indexing
// PR c++/113798
// { dg-do compile { target c++26 } }
// From LLVM's cxx2c-pack-indexing.cpp.

template <class, class>
constexpr bool is_same = false;
template <class T>
constexpr bool is_same<T, T> = true;

template <typename T>
constexpr bool
f (auto&&... p)
{
  return is_same<T, decltype(p...[0])>;
}

void
g ()
{
    int a = 0;
    const int b = 0;
    static_assert(f<int&&>(0));
    static_assert(f<int&>(a));
    static_assert(f<const int&>(b));
}

template<auto... p>
struct A {
  enum E {
    x = p...[0]
  };
};
static_assert(A<42>::x == 42);

struct S { };
template<auto... p>
constexpr auto constant_initializer = p...[0];
constexpr auto InitOk = constant_initializer<S{}>;

consteval int evaluate(auto... p) {
    return p...[0];
}
constexpr int x = evaluate(42, S{});
static_assert(x == 42);

template <auto... Is>
struct IL{};

template <typename... Ts>
struct TL{};

template <typename Tl, typename Il>
struct SpliceImpl;

template <typename... Ts, auto... Is>
struct SpliceImpl<TL<Ts...>, IL<Is...>>
{
    using type = TL<Ts...[Is]...>;
};

template <typename Tl, typename Il>
using Splice = typename SpliceImpl<Tl, Il>::type;
using type = Splice<TL<char, short, long, double>, IL<1, 2>>;
static_assert(is_same<type, TL<short, long>>);
