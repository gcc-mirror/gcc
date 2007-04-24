// { dg-options "-std=gnu++98 -pedantic-errors" }
template<typename... Args> class tuple; // { dg-error "variadic templates" }
