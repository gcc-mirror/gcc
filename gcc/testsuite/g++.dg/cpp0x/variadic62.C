// { dg-options "-pedantic-errors" }
template<typename... Args> class tuple; // { dg-error "variadic templates" }
