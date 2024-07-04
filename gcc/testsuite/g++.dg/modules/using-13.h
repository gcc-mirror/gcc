// Like using-11.h, but additional kinds of declarations.

struct A {};

template <typename> struct B {};
template <> struct B<int> { using foo = int; };
template <typename T> struct B<T*> { using bar = T; };

using C = int;

inline int D = 0;

#if __cpp_concepts >= 201907L
template <typename>
concept E = true;
#endif
