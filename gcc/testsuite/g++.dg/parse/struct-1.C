// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
// PR c++/18731

template<typename T> struct T::A {}; // { dg-error "invalid class name" }
