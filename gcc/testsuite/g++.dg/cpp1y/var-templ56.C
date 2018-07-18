// PR c++/82085
// { dg-do compile { target c++14 } }

template <const char& V>
using char_sequence_t = int;

template <typename T>
constexpr char name_of_v = 'x';

template <typename T>
using type = char_sequence_t<name_of_v<T>>;
