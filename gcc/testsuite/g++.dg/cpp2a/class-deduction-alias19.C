// PR c++/114377
// { dg-do compile { target c++20 } }

template <template <typename> typename Iterator>
struct K {};

template <typename C, typename IteratorPolicy>
class Foo {};

template <typename C, template<typename> typename TTP>
using Bar = Foo<C, K<TTP>>;

void s() {
    Bar(1); // { dg-error "failed|no match" }
}
