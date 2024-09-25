// PR c++/67007
// { dg-do compile { target c++20 } }

template <class U>
concept A =
  requires (U u) { u; };

template <class T>
concept B =
  requires (T t) { { t } -> A; };

void foo(B auto);
