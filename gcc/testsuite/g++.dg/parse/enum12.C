// PR c++/67847

template < typename T > 
class D
{
  enum T::Color {R, G, B} c; // { dg-error "nested name specifier" }
};
