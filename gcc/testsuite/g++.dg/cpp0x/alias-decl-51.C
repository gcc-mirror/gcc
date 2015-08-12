// PR c++/66743
// { dg-do compile { target c++11 } }

template< class T >
struct
  type_is { using type = T; };

template< class T >
  using underlying_type = type_is<__underlying_type(T)>;
