// { dg-do compile { target c++11 } }
template<typename T, T... Values>
struct vector_c { };

vector_c<int, 17, 42> intvec;
vector_c<char, 'a', 'b', 'c'> charvec;
