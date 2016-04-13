// PR c++/67084
// { dg-options "-std=c++1z -fconcepts" }

template <class T>
constexpr bool p = false;

template <class T>
constexpr bool p<T*> = false;

template <class T>
  requires true
constexpr bool p<T*> = false;

template <class T>
  requires true && T() == 0
constexpr bool p<T*> = true;

template <class T>
constexpr bool q = false;

template <class T>
constexpr bool q<T*> = true;

template <class T>
  requires false
constexpr bool q<T*> = false;

template <class T>
  requires false && T() != 0
constexpr bool q<T*> = false;

static_assert (p<int*>,"");
