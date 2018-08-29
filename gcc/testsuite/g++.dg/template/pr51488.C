// PR c++/51488

template<class T,class U=void> struct s;
template<class T> struct s<T,typename s<T>::a> {};
s<int> ca;  // { dg-error "" }
