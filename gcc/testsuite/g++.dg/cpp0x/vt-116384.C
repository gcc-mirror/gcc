// PR c++/116384
// { dg-do compile { target c++11 } }

namespace a {
template <bool, typename> struct c;
template <typename> struct d;
}
namespace e {
namespace g {
template <typename> using h = void;
template <typename, template <typename> class, typename...> struct detector {};
template <template <typename> class i, typename... args>
struct detector<h<i<args...>>, i, args...>;
}
template <template <typename> class i, typename... args>
using j = g::detector<void, i, args...>;
template <bool b, typename k = void> using l = typename a::c<b, k>::m;
template <typename> struct conjunction;
namespace g {
template <typename k> using n = l<conjunction<a::d<k>>::p>;
}
template <typename k, g::n<k> = true> class o;
}
struct r;
template <typename k> using q = e::o<k>;
void s() { e::j<q, r> f; }
