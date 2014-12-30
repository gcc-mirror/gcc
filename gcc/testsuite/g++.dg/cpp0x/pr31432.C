// { dg-do compile { target c++11 } }
template<typename..., typename> struct A // { dg-error "parameter pack" }
{
 static int i;
};

A<int, int> a;
A<char,int> b;
