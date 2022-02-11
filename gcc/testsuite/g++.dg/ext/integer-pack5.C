// PR c++/94490
// { dg-do compile { target c++14 } }

template<class T>
constexpr int fake_tuple_size_v = 3;
template<int...> struct intseq {};

// So that it compiles with clang++.
#if __has_builtin(__make_integer_seq)
using size_t = decltype(sizeof(1));
template<typename, size_t... _Indices>
using _IdxTuple = intseq<_Indices...>;

template<int N> using genseq = __make_integer_seq<_IdxTuple, size_t, N>;
#else
template<int N> using genseq = intseq<__integer_pack(N)...>;
#endif

template<int A, class S = genseq<0 ? A : A>>
struct arith_result
{ };

template<typename T>
auto Mul(const T&)
{
    return [](auto) { return arith_result<fake_tuple_size_v<T>> { }; }(0);
}

auto x = Mul(0);
