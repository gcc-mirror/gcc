// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

typedef int size_t;
template <typename _Tp> struct A { static constexpr _Tp value = 1; };
template <typename _Tp> _Tp declval();
template <typename _From, typename _To> struct __is_convertible_helper {
  template <typename, typename> static A<bool> __test(int);
  typedef decltype(__test<_From, _To>(0)) type;
};
template <typename, typename>
struct is_convertible : __is_convertible_helper<int, int>::type {};
template <typename> struct remove_reference;
template <typename _Tp> struct remove_reference<_Tp &> { typedef _Tp type; };
struct base;
struct general;
template <typename _Tp, _Tp...> struct B;
template <typename _Tp, _Tp> using make_integer_sequence = B<int>;
template <size_t... _Idx> using index_sequence = B<size_t, _Idx...>;
template <size_t _Num>
using make_index_sequence = make_integer_sequence<size_t, _Num>;
template <bool...> struct and_c_impl { static constexpr bool value = true; };
template <bool...> constexpr bool and_c() { return and_c_impl<>::value; }

template <class X, class Y> concept cpt_Convertible =
  is_convertible<X, Y>::value;

template <class T> using uncvref_t = typename remove_reference<T>::type;
struct Plus;
using index_t = int;
template <class> bool cpt_Index;
template <class... Extents>
requires (and_c<cpt_Index<Extents>()...>()) class Dimensionality;
namespace detail_concept {
template <class> bool match_dimensionality;
template <class... Extents>
constexpr bool match_dimensionality<Dimensionality<Extents...>> = true;
}
template <class X> concept cpt_Dimensionality =
  detail_concept::match_dimensionality<X>;

template <class X> concept cpt_Shaped = requires(X x){{x};};

template <class X> concept cpt_Dimensioned = cpt_Shaped<X>;

template <class... Extents>
requires (and_c<cpt_Index<Extents>()...>()) class Dimensionality {
public:
  static constexpr size_t num_dimensions = sizeof...(Extents);
};
template <index_t...> using DimensionalityC = Dimensionality<>;
template <class> struct dimensionality_type_impl;
template <cpt_Dimensioned X> struct dimensionality_type_impl<X> {
  using type = uncvref_t<decltype(declval<X>().dimensionality())>;
};
template <cpt_Dimensioned X>
using dimensionality_type = typename dimensionality_type_impl<X>::type;
template <class Functor, class... Expressibles>
requires requires(Functor functor, Expressibles... expressibles) {
  map_expressions_impl(functor, expressibles...);
}

decltype(auto) map_impl(Functor, Expressibles...);
void cpt_ContinualScalar();
template <class> concept cpt_Scalar = cpt_ContinualScalar;

template <class X> concept cpt_FlatEvaluator =
  requires(X x){{x}->cpt_Scalar;};

template <class, class> bool k_evaluator_impl;
template <size_t... Indexes, class Evaluator>
constexpr bool k_evaluator_impl<index_sequence<Indexes...>, Evaluator> = true;
template <class X, size_t K> concept cpt_KEvaluator =
  k_evaluator_impl<make_index_sequence<K>, X>;

template <class X, size_t K> concept cpt_KCompatibleEvaluator =
  cpt_KEvaluator<X, K>;

template <class X> concept cpt_Structure =
  cpt_Convertible<X, base>;

template <cpt_Dimensionality Dimensionality, cpt_Structure,
          cpt_KCompatibleEvaluator<Dimensionality::num_dimensions> Evaluator>
class NumericArrayExpression;
namespace detail_concept {

template <class> bool match_numeric_array_expression;

template <cpt_Dimensionality Dimensionality,
          cpt_Structure Structure,
          cpt_KCompatibleEvaluator<Dimensionality::num_dimensions> Evaluator>
constexpr bool match_numeric_array_expression<
    NumericArrayExpression<Dimensionality, Structure, Evaluator>> = true;

}
template <class X> concept cpt_NumericArrayExpression =
  detail_concept::match_numeric_array_expression<X>;

namespace expression_traits {
namespace detail_expression_traits {
template <class...> struct first_numeric_array_expression_impl;
template <cpt_NumericArrayExpression ExpressionFirst, class... ExpressionsRest>
struct first_numeric_array_expression_impl<ExpressionFirst,
                                           ExpressionsRest...> {
  using type = ExpressionFirst;
};
}
template <class... Expressions>
using first_numeric_array_expression =
    typename detail_expression_traits::first_numeric_array_expression_impl<
        Expressions...>::type;
template <class... Expressions>
using first_expression_dimensionality =
    dimensionality_type<first_numeric_array_expression<Expressions...>>;
}
template <cpt_Dimensionality Dimensionality, cpt_Structure,
          cpt_KCompatibleEvaluator<Dimensionality::num_dimensions> Evaluator>
class NumericArrayExpression {
public:
  NumericArrayExpression(Dimensionality, Evaluator) {}
  Dimensionality &dimensionality();
};

template <cpt_Structure Structure, cpt_Dimensionality Dimensionality,
          cpt_KCompatibleEvaluator<Dimensionality::num_dimensions> Evaluator>
auto make_numeric_array_expression(Dimensionality dimensionality,
                                   Evaluator evaluator) {
  return NumericArrayExpression<Dimensionality, Structure, Evaluator>(
      dimensionality, evaluator);
}

template <size_t, class Functor, class... Evaluators>
auto make_map_evaluator_impl(Functor) requires
    (and_(cpt_FlatEvaluator<Evaluators>...));
template <class Functor, class... Expressions>
requires
requires(Expressions... expressions,
         expression_traits::first_expression_dimensionality<Expressions...>
             dimensionality) {
  make_map_evaluator_impl<decltype(dimensionality)::num_dimensions>(
      expressions...);
}

decltype(auto) map_expressions_impl(Functor, Expressions...);
template <class Functor, class... Expressibles> concept cpt_Mappable =
  requires(Functor functor, Expressibles... expressibles) {
    map_impl(functor, expressibles...);
  };

void ____C_A_T_C_H____T_E_S_T____8() {
  auto e1 = make_numeric_array_expression<general>(DimensionalityC<>(), [] {});
  using E1 = decltype(e1);
  cpt_Mappable<Plus, E1>;
}
