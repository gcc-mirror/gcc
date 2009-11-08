// { dg-options "-std=gnu++0x" }
template<typename ... Elements> class Tuple;
Tuple<>* t; // OK: Elements is empty
Tuple* u; // { dg-error "template-name" }
