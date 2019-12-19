template<typename> struct A; // { dg-message "27:declaration" }
template<typename T> A<T>::A(); // { dg-error "22:invalid use of incomplete type" }
