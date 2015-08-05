// { dg-do compile { target c++14 } }

template<typename T>
bool V1 = true;

template<typename T>
bool V1<int> = false; // { dg-error "primary template|not deducible" }
