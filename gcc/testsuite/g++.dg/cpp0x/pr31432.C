// { dg-options "-std=gnu++0x" }
template<typename..., typename> struct A // { dg-error "parameter pack" }
{
 static int i;
};

A<int, int> a; // { dg-error "mismatch|expected|invalid type" }
A<char,int> b; // { dg-error "mismatch|expected|invalid type" }
