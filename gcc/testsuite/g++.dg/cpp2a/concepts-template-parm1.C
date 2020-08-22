// { dg-do compile { target c++20 } }

template<typename T>
concept Int = __is_same_as(T, int);

// Type template parameters
template<Int T = int> struct S1 { };
template<Int = int> struct S2;
template<Int T> struct S2 { };

// Non-type template parameters
template<Int auto N = 0> struct S3 { };
template<Int auto = 0> struct S4;
template<Int auto N> struct S4 { };

S1<> s1;
S2<> s2;
S3<> s3;
S4<> s4;
