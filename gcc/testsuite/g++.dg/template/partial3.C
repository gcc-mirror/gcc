// { dg-do compile }
// Origin: John Maddock <john at johnmaddock dot co dot uk>
// PR c++/13997: Error while matching partial specialization of array type

template <typename T>
struct is_array;

template <typename T, unsigned int N>
struct is_array<T[N]>;

template <typename T, unsigned int N>
struct is_array<const T[N]> {};

template struct is_array<int const[2]>;
