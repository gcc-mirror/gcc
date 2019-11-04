// PR c++/67084
// { dg-do compile { target c++17 } }
// { dg-additional-options "-fconcepts" }

template <class T>
concept True = true;
template <class T>
concept False = false;

template <class T>
constexpr bool p = false;

template <class T>
constexpr bool p<T*> = false;

template <class T>
  requires True<T>
constexpr bool p<T*> = false;

template <class T>
  requires True<T> && (T() == 0)
constexpr bool p<T*> = true;

template <class T>
constexpr bool q = false;

template <class T>
constexpr bool q<T*> = true;

template <class T>
  requires False<T>
constexpr bool q<T*> = false;

template <class T>
  requires False<T> && (T() != 0)
constexpr bool q<T*> = false;

static_assert (p<int*>,"");
