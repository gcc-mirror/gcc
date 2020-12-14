// { dg-do compile { target c++20 } }

// Here, normal overload resolution would consider B::operator bool when
// evaluating A(b), leading to a hard error instantiating Error<int>, but we
// avoid considering it by noticing that converting bool (a scalar) to A (a
// class) would require a user-defined conversion, which is not allowed when
// we're already dealing with the user-defined conversion to A.

// This seems to be allowed by [temp.inst]/9: "If the function selected by
// overload resolution (12.4) can be determined without instantiating a class
// template definition, it is unspecified whether that instantiation actually
// takes place."

template <class T>
struct Error { static constexpr auto value = T::value; };

struct A { A(const A&); };

template <class T>
struct B { operator bool() requires Error<T>::value; };

template <class T>
concept C = requires (B<T> b) { A(b); };

static_assert(!C<int>);
