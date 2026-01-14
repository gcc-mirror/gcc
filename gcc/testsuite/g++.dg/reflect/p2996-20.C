// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <functional>
#include <meta>
#include <utility>
#include <vector>
#include <initializer_list>
#include <tuple>

template<std::pair<std::size_t, std::size_t>... indices>
struct Indexer {
   template<typename Tuples>
   // Can use tuple indexing instead of tuple of tuples
   constexpr auto operator()(Tuples&& tuples) const {
     using ResultType = std::tuple<
       std::tuple_element_t<
         indices.second,
         std::remove_cvref_t<std::tuple_element_t<indices.first, std::remove_cvref_t<Tuples>>>
       >...
     >;
     return ResultType(std::get<indices.second>(std::get<indices.first>(std::forward<Tuples>(tuples)))...);
   }
};

template <class T>
consteval auto subst_by_value(std::meta::info tmpl, std::vector<T> args)
    -> std::meta::info
{
    std::vector<std::meta::info> a2;
    for (T x : args) {
        a2.push_back(std::meta::reflect_constant(x));
    }

    return substitute(tmpl, a2);
}

consteval auto make_indexer(std::vector<std::size_t> sizes)
    -> std::meta::info
{
    std::vector<std::pair<int, int>> args;

    for (std::size_t tidx = 0; tidx < sizes.size(); ++tidx) {
        for (std::size_t eidx = 0; eidx < sizes[tidx]; ++eidx) {
            args.push_back({tidx, eidx});
        }
    }

    return subst_by_value(^^Indexer, args);
}

template<typename... Tuples>
constexpr auto my_tuple_cat(Tuples&&... tuples) {
    constexpr typename [: make_indexer({tuple_size(remove_cvref(^^Tuples))...}) :] indexer;
    return indexer(std::forward_as_tuple(std::forward<Tuples>(tuples)...));
}

int r;
constexpr auto x = my_tuple_cat(std::make_tuple(10, std::ref(r)), std::make_tuple(21.0, 22, 23, 24));
static_assert(std::same_as<decltype(x), const std::tuple<int, int&, double, int, int, int>>);
static_assert(std::get<5>(x) == 24);
