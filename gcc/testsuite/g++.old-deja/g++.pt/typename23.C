// { dg-do assemble  }
template<class T>
void value_type(T){}

template <class T>
struct B{
  typedef T value_type;
};

template<class>class Vector{};

template<class T>
struct D:B<T>{
  Vector<value_type> r;  // { dg-error "" } value_type is not a type
};
