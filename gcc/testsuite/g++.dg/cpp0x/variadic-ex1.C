// { dg-options "-std=gnu++11" }
template<typename ... Elements> class Tuple;
Tuple<>* t; // OK: Elements is empty
Tuple* u; // { dg-error "template-name" }
