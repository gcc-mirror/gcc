// PR c++/47144

template<typename> struct A { };
A< struct B { }* >::SomeNonSense // { dg-error "types may not be defined" }
int y;

// { dg-prune-output "SomeNonSense" }
