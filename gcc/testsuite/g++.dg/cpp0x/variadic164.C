// PR c++/63522
// { dg-do compile { target c++11 } }

template <typename...> struct tuple;
template <typename...> void slice();
template <int Index, typename...> using slice_result = decltype(slice<Index>);
template <typename Tuple, typename... Tuples, int... ElementIndices,
          typename =
              typename tuple<slice_result<ElementIndices, Tuples...>,
                             slice_result<ElementIndices, Tuples...>...>::type> // { dg-error "parameter pack" }
void zip_with(Tuple...);	// { dg-warning "omission of ',' before varargs '...' is deprecated" "" { target c++26 } }
decltype(zip_with(0)) d;	// { dg-error "no match" }
