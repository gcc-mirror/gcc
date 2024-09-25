// PR c++/67427
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

template <class S, class I>
concept Sentinel =
  requires (I i) { i; };

template <class I, class S>
concept SizedIteratorRange =
  Sentinel<S, I> && true;

template<typename S, typename I>
  requires Sentinel<S, I>
void distance(I first, S last) {}

template <class I, class S>
  requires SizedIteratorRange<I, S>
void distance(I first, S last) {}

int main() {
  distance(42, 43); // { dg-error "ambiguous" }
}
