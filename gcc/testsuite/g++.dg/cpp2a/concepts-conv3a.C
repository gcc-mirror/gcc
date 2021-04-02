// { dg-do compile { target c++20 } }

// But make sure we do consider template conversions that could produce the
// right type.

template <class T>
struct Error { static constexpr auto value = T::value; }; // { dg-error "not a member" }

struct A { A(const A&); };

template <class T>
struct B { template <class U> operator U() requires Error<T>::value; };

template <class T>
concept C = requires (B<T> b) { A(b); }; // { dg-message "required from here" }

static_assert(!C<int>);
