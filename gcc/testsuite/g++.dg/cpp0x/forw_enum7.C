// { dg-do compile }
// { dg-options "-std=c++0x" }

template<typename T> struct S1
{
    enum E1 : int;
    enum E1 : T;
    enum class E2 : int;
    enum class E2 : T;
};

template<typename T> enum S1<T>::E1 : int { e1 };
template<typename T> enum class S1<T>::E2 : T { e2 };

S1<int>::E1 x1 = S1<int>::e1;
S1<int>::E1 x11 = S1<int>::E1::e1;
S1<int>::E2 x2 = S1<int>::E2::e2;

enum S1<int>::E1 ex1 = S1<int>::e1;
enum S1<int>::E1 ex11 = S1<int>::E1::e1;
enum S1<int>::E2 ex2 = S1<int>::E2::e2;
