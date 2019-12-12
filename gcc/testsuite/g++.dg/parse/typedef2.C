template <typename T> struct B { typedef typename T::X X; };
template <typename T> struct A { typedef B<T>::X::Y Z; }; // { dg-error "before 'B<T>::X::Y' because 'B<T>::X'" "" { target c++17_down } }
