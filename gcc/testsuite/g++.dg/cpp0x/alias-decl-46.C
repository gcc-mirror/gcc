// PR c++/62134
// { dg-do compile { target c++11 } }

template<typename... T> struct tuple;
template<__SIZE_TYPE__, typename U> struct tuple_element;

template<bool, typename T = void> struct enable_if  { };
template<typename T> struct enable_if<true, T> { typedef T type; };

template <int V> struct int_t { static constexpr int value = V; };
template <int V> int constexpr int_t<V>::value;

template <class A, class Val, int i=0>
struct Index
{
    static int const value = -1;
};

template <class ... A, class Val, int i>
struct Index<tuple<Val, A ...>, Val, i>
{
    static int const value = i;
};

template <class A0, class ... A, class Val, int i>
struct Index<tuple<A0, A ...>, Val, i>
{
    static int const value = Index<tuple<A ...>, Val, i+1>::value;
};

template <class C, class R> struct PermutationSign;

template <int w, class C, class R>
struct PermutationSignIfFound
{
    static int const value = 0;
};

template <class C, class R>
struct PermutationSignIfFound<-1, C, R>
{
    static int const value = 0;
};

template <>
struct PermutationSign<tuple<>, tuple<>>
{
    static int const value = 1;
};

template <class C>
struct PermutationSign<C, tuple<>>
{
    static int const value = 0;
};

template <class R>
struct PermutationSign<tuple<>, R>
{
    static int const value = 0;
};

template <class C, class Org>
struct PermutationSign
{
    static int const value
    = PermutationSignIfFound
      <Index<C, typename tuple_element<0, Org>::type>::value,
       C, Org>::value;
};

template <class A, template <class> class Pred, int i=0, class Enable=void>
struct IndexIf
{
    static int const value = -1;
    using type = tuple<>;
};

template <class A0, class ... A, template <class> class Pred, int i>
struct IndexIf<tuple<A0, A ...>, Pred, i,
	       typename enable_if<Pred<A0>::value>::type>
{
    using type = A0;
    static int const value = i;
};

template <class A0, class ... A, template <class> class Pred, int i>
struct IndexIf<tuple<A0, A ...>, Pred, i,
	       typename enable_if<!Pred<A0>::value>::type>
{
    using next = IndexIf<tuple<A ...>, Pred, i+1>;
    using type = typename next::type;
    static int const value = next::value;
};

template <class P>
struct MatchPermutationP
{
    template <class A> using type = PermutationSign<P, A>;
};

template <class P, class Plist> struct FindCombination
{
    using type = IndexIf<Plist, MatchPermutationP<P>::template type>;
    static int const where = type::value;
    static int const sign
    = (where>=0) ? PermutationSign<P, typename type::type>::value : 0;
};

int main()
{
  using finder = FindCombination<tuple<>, tuple<tuple<>>>;
  static_assert(finder::where==0 && finder::sign==+1, "bad");
}
