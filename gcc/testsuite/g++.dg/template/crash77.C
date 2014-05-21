// PR c++/34603

template<typename> struct A; // { dg-message "declaration" }

template<typename T> A<T>::A( struct A; // { dg-error "definition|expected|incomplete" }
