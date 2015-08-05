// PR c++/66595
// { dg-do compile { target c++14 } }

template<typename T> int typeID{42};
template<typename T> double typeID<double>{10.10}; // { dg-error "primary template|redeclaration|not deducible" }
