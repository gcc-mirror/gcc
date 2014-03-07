// { dg-do compile { target c++11 } }
template<typename T, T... Values>
struct vector_c { };

vector_c<int, 1, 2, 3> v1;
vector_c<char, 'a', 'b', 'c'> v2;
vector_c<long, 1u, 2, 3l> v3;
