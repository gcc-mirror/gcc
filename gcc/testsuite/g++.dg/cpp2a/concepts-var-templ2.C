// PR c++/103341
// { dg-do compile { target c++20 } }

template<class T, class U> concept same_as = __is_same(T, U);
template<class T> same_as<T> auto v1a = 1;
template<class T> same_as<T> auto v1b = T();
template<class T> same_as<T*> auto v2a = 1; // { dg-error "constraints" }
template<class T> same_as<T*> auto v2b = T(); // { dg-error "constraints" }

template int v1a<int>;
template int v1b<int>;
template int v2a<int>; // { dg-message "required from here" }
template int v2b<int>; // { dg-message "required from here" }
