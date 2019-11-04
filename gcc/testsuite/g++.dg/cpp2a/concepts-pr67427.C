// PR c++/67427
// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

template <class S, class I>
concept bool Sentinel =
  requires (I i) { i; };

template <class I, class S>
concept bool SizedIteratorRange =
  Sentinel<S, I> && true;

Sentinel{S, I}
void distance(I first, S last) {}

template <class I, class S>
  requires SizedIteratorRange<I, S>
void distance(I first, S last) {}

int main() {
  distance(42, 43); // { dg-error "ambiguous" }
}
