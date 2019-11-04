// PR c++/67007
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template <class U>
concept bool A =
  requires (U u) { u; };

template <class T>
concept bool B =
  requires (T t) { { t } -> A; };

void foo(B);
