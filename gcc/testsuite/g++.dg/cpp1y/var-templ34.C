// PR c++/66427
// { dg-do compile { target c++14 } }

template<typename T>
struct complex {};

struct plus {};
struct multiplies {};

template<typename T, typename Op>
constexpr T identity_element;

template<>
constexpr int identity_element<int, plus> = 0;

template<typename T>
constexpr complex<T> identity_element<complex<T>, plus> = {
    identity_element<T, plus>,
    identity_element<T, plus>
};
