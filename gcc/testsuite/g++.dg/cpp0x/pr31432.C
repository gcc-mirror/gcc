// { dg-do compile { target c++11 } }
template<typename..., typename> struct A // { dg-error "parameter pack" }
{
 static int i;
};

A<int, int> a; // { dg-error "mismatch|expected|invalid type" }
A<char,int> b; // { dg-error "mismatch|expected|invalid type" }
