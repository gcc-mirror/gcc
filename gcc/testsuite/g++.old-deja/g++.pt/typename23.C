// Build don't link:
template<class T>
void value_type(T){}

template <class T>
struct B{
  typedef T value_type;
};

template<class>class Vector{};

template<class T>
struct D:B<T>{
  Vector<value_type> r;  // ERROR - value_type is not a type
};
