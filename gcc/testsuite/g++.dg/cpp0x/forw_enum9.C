// { dg-do compile }
// { dg-options "-std=c++0x" }

template<typename T> struct S1
{
    enum E1 : int;
    enum class E2 : int;
};

template<typename T> enum S1<T>::E1 : int { e1 };
template<typename T> enum class S1<T>::E2 : T { e2 };

template<> enum S1<int>::E1 : int { i1 };
template<> enum class S1<int>::E2 : int { i2 };

S1<char>::E1 xci = S1<char>::e1;
S1<int>::E1 xi1 = S1<int>::i1;

S1<char>::E2 xc2 = S1<char>::E2::e2;
S1<int>::E2 xi2 = S1<int>::E2::i2;

